using Microsoft.Data.SqlClient;

namespace CoTAS.Storage;

/// <summary>
/// MSSQL-backed storage engine for TAS file operations.
/// Preloads all DDF .int files at startup. If a table is requested without a .int mapping,
/// generates schema dynamically from MSSQL INFORMATION_SCHEMA with defaults.
/// </summary>
public sealed class StorageEngine : IDisposable
{
    private readonly StorageConfig _config;
    private readonly Dictionary<int, FileHandle> _openFiles = new();
    private readonly Dictionary<string, SqlConnection> _connections = new(StringComparer.OrdinalIgnoreCase);

    // Preloaded schemas keyed by TABLE_NAME (case-insensitive)
    private readonly Dictionary<string, TableSchema> _schemas = new(StringComparer.OrdinalIgnoreCase);

    public StorageEngine(StorageConfig config)
    {
        _config = config;
        PreloadAllDdfFiles();
    }

    /// <summary>
    /// Legacy constructor for backward compatibility with CLI.
    /// ddfDirectory can be a parent dir (db/) or a specific dir (db/GPACIFIC).
    /// </summary>
    public StorageEngine(string connectionString, string ddfDirectory)
    {
        _config = new StorageConfig
        {
            BaseConnectionString = connectionString,
            DdfDirectory = ddfDirectory,
        };
        PreloadAllDdfFiles();
    }

    /// <summary>
    /// Load all .int files from all DDF directories at startup.
    /// Scans DdfDirectory and all its subdirectories (GPACIFIC, JADVDATA, etc.).
    /// </summary>
    private void PreloadAllDdfFiles()
    {
        var parser = new DdfParser();
        foreach (var ddfDir in _config.GetAllDdfDirectories())
        {
            foreach (var intFile in Directory.GetFiles(ddfDir, "*.int"))
            {
                try
                {
                    var schema = parser.Parse(intFile);
                    schema = schema with { SchemaName = _config.DefaultSchema };
                    _schemas[schema.TableName] = schema;
                }
                catch
                {
                    // Skip unparseable .int files
                }
            }
        }
    }

    /// <summary>
    /// Get all preloaded schemas.
    /// </summary>
    public IReadOnlyDictionary<string, TableSchema> Schemas => _schemas;

    /// <summary>
    /// Get or open a SQL connection for a specific database space name.
    /// </summary>
    private async Task<SqlConnection> GetConnectionAsync(string databaseSpaceName)
    {
        string dbName = _config.ResolveDatabaseName(databaseSpaceName);

        if (_connections.TryGetValue(dbName, out var existing) && existing.State == System.Data.ConnectionState.Open)
            return existing;

        existing?.Dispose();
        var conn = new SqlConnection(_config.GetConnectionString(databaseSpaceName));
        await conn.OpenAsync();
        _connections[dbName] = conn;
        return conn;
    }

    /// <summary>
    /// Get a connection for the default database.
    /// </summary>
    private Task<SqlConnection> GetDefaultConnectionAsync()
    {
        string defaultDb = _config.DatabaseMap.Values.FirstOrDefault() ?? "gpacific";
        return GetConnectionAsync(defaultDb);
    }

    /// <summary>
    /// Open a TAS file by name and file number. Uses preloaded DDF if available,
    /// otherwise queries MSSQL to generate schema dynamically.
    /// </summary>
    public async Task<FileHandle> OpenFileAsync(string tasFileName, int fileNumber, string lockMode = "NONE")
    {
        if (_openFiles.ContainsKey(fileNumber))
            throw new StorageException($"File number {fileNumber} is already open");

        TableSchema schema;
        if (_schemas.TryGetValue(tasFileName, out var preloaded))
        {
            schema = preloaded;
        }
        else
        {
            // No .int mapping — try to discover schema from MSSQL
            schema = await DiscoverSchemaFromSqlAsync(tasFileName);
            _schemas[tasFileName] = schema;
        }

        // Ensure the table exists in the correct database
        var conn = await GetConnectionAsync(schema.DatabaseSpace);
        await EnsureTableExistsAsync(conn, schema);

        var handle = new FileHandle
        {
            FileNumber = fileNumber,
            Schema = schema,
            TasFileName = tasFileName,
            LockMode = lockMode,
        };
        handle.Buffer.Clear(schema);
        _openFiles[fileNumber] = handle;
        return handle;
    }

    /// <summary>
    /// Discover table schema from MSSQL INFORMATION_SCHEMA when no .int file exists.
    /// If the table doesn't exist yet, creates a minimal schema with defaults.
    /// </summary>
    private async Task<TableSchema> DiscoverSchemaFromSqlAsync(string tableName)
    {
        // Try each known database to find the table
        foreach (var dbName in _config.DatabaseMap.Values.Distinct(StringComparer.OrdinalIgnoreCase))
        {
            try
            {
                var conn = await GetConnectionAsync(dbName);
                string checkSql = @"
                    SELECT COUNT(*) FROM INFORMATION_SCHEMA.TABLES
                    WHERE TABLE_SCHEMA = @schema AND TABLE_NAME = @table";

                using var checkCmd = new SqlCommand(checkSql, conn);
                checkCmd.Parameters.AddWithValue("@schema", _config.DefaultSchema);
                checkCmd.Parameters.AddWithValue("@table", tableName);
                int count = (int)(await checkCmd.ExecuteScalarAsync())!;

                if (count == 0) continue;

                // Table found — read column definitions
                string colSql = @"
                    SELECT COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH,
                           NUMERIC_PRECISION, COLUMN_DEFAULT, ORDINAL_POSITION
                    FROM INFORMATION_SCHEMA.COLUMNS
                    WHERE TABLE_SCHEMA = @schema AND TABLE_NAME = @table
                    ORDER BY ORDINAL_POSITION";

                using var colCmd = new SqlCommand(colSql, conn);
                colCmd.Parameters.AddWithValue("@schema", _config.DefaultSchema);
                colCmd.Parameters.AddWithValue("@table", tableName);

                var fields = new List<FieldDefinition>();
                using var reader = await colCmd.ExecuteReaderAsync();
                while (await reader.ReadAsync())
                {
                    string colName = reader.GetString(0);
                    string dataType = reader.GetString(1);
                    int? maxLen = reader.IsDBNull(2) ? null : (int?)reader.GetInt32(2);

                    var nativeType = dataType.ToUpperInvariant() switch
                    {
                        "NVARCHAR" or "VARCHAR" or "CHAR" or "NCHAR" or "TEXT" or "NTEXT" => NativeType.String,
                        "INT" or "BIGINT" => NativeType.Integer,
                        "SMALLINT" or "TINYINT" => NativeType.SmallInt,
                        "FLOAT" or "REAL" or "DECIMAL" or "NUMERIC" or "MONEY" or "SMALLMONEY" => NativeType.Numeric,
                        "DATE" or "DATETIME" or "DATETIME2" or "SMALLDATETIME" => NativeType.Date,
                        _ => NativeType.String,
                    };

                    fields.Add(new FieldDefinition
                    {
                        FieldNumber = reader.GetInt32(5),
                        Name = colName,
                        NativeType = nativeType,
                        NativeLength = maxLen ?? (nativeType == NativeType.String ? 50 : 4),
                        NativeOffset = 0,
                    });
                }

                return new TableSchema
                {
                    DatabaseSpace = dbName,
                    TableName = tableName,
                    SchemaName = _config.DefaultSchema,
                    NumberOfFields = fields.Count,
                    Fields = fields,
                    Indexes = [],
                };
            }
            catch
            {
                // Try next database
            }
        }

        // Table not found anywhere — create a minimal placeholder schema in the first database
        string defaultDb = _config.DatabaseMap.Values.FirstOrDefault() ?? "gpacific";
        return new TableSchema
        {
            DatabaseSpace = defaultDb,
            TableName = tableName,
            SchemaName = _config.DefaultSchema,
            NumberOfFields = 0,
            Fields = [],
            Indexes = [],
        };
    }

    /// <summary>
    /// Get an open file handle by file number.
    /// </summary>
    public FileHandle GetFile(int fileNumber)
    {
        if (_openFiles.TryGetValue(fileNumber, out var handle))
            return handle;
        throw new StorageException($"File number {fileNumber} is not open");
    }

    /// <summary>
    /// Get an open file handle by TAS file name (case-insensitive).
    /// </summary>
    public FileHandle? GetFileByName(string tasFileName)
    {
        return _openFiles.Values.FirstOrDefault(f =>
            f.TasFileName.Equals(tasFileName, StringComparison.OrdinalIgnoreCase));
    }

    /// <summary>
    /// Close a file handle.
    /// </summary>
    public void CloseFile(int fileNumber)
    {
        _openFiles.Remove(fileNumber);
    }

    /// <summary>
    /// Find a record. FindType: M=match, G=greater, F=first, L=last, N=next, P=previous.
    /// </summary>
    public async Task FindAsync(FileHandle handle, string findType, int indexNumber,
        string? keyValue = null, string? errLabel = null)
    {
        var conn = await GetConnectionAsync(handle.Schema.DatabaseSpace);
        var schema = handle.Schema;
        string tableName = QuoteName(schema.SchemaName) + "." + QuoteName(schema.TableName);

        string sql;
        var parameters = new List<SqlParameter>();

        switch (findType.ToUpperInvariant())
        {
            case "F": // First record
                sql = $"SELECT TOP 1 * FROM {tableName} ORDER BY (SELECT NULL)";
                break;

            case "L": // Last record
                sql = $"SELECT TOP 1 * FROM {tableName} ORDER BY (SELECT NULL) DESC";
                break;

            case "M": // Match by key
            case "G": // Greater or equal by key
            {
                var index = schema.Indexes.FirstOrDefault(i => i.IndexNumber == indexNumber);
                if (index == null || string.IsNullOrEmpty(keyValue))
                {
                    handle.IsEof = true;
                    handle.HasRecord = false;
                    handle.LastError = 1;
                    return;
                }

                var (whereClause, whereParams) = BuildKeyWhere(schema, index, keyValue, findType == "G" ? ">=" : "=");
                sql = $"SELECT TOP 1 * FROM {tableName} WHERE {whereClause}";
                parameters.AddRange(whereParams);
                break;
            }

            default: // N (next), P (previous)
                sql = $"SELECT TOP 1 * FROM {tableName}";
                break;
        }

        using var cmd = new SqlCommand(sql, conn);
        foreach (var p in parameters)
            cmd.Parameters.Add(p);

        using var reader = await cmd.ExecuteReaderAsync();
        if (await reader.ReadAsync())
        {
            handle.Buffer.LoadFromReader(reader, schema);
            handle.HasRecord = true;
            handle.IsEof = false;
            handle.LastError = 0;
        }
        else
        {
            handle.HasRecord = false;
            handle.IsEof = true;
            handle.LastError = 1;
        }
    }

    /// <summary>
    /// Save the current record buffer. INSERT if new, UPDATE if existing.
    /// </summary>
    public async Task SaveAsync(FileHandle handle)
    {
        var conn = await GetConnectionAsync(handle.Schema.DatabaseSpace);
        var schema = handle.Schema;
        string tableName = QuoteName(schema.SchemaName) + "." + QuoteName(schema.TableName);
        var values = handle.Buffer.GetAll();

        if (handle.Buffer.IsNew)
        {
            var columns = schema.Fields.Select(f => QuoteName(f.Name)).ToList();
            var paramNames = schema.Fields.Select((f, i) => $"@p{i}").ToList();
            string sql = $"INSERT INTO {tableName} ({string.Join(", ", columns)}) VALUES ({string.Join(", ", paramNames)})";

            using var cmd = new SqlCommand(sql, conn);
            for (int i = 0; i < schema.Fields.Count; i++)
            {
                var field = schema.Fields[i];
                object? val = values.TryGetValue(field.Name, out var v) ? v : DBNull.Value;
                cmd.Parameters.AddWithValue($"@p{i}", val ?? DBNull.Value);
            }
            await cmd.ExecuteNonQueryAsync();
            handle.Buffer.IsNew = false;
            handle.Buffer.IsDirty = false;
        }
        else
        {
            var index = schema.Indexes.FirstOrDefault();
            if (index == null)
                throw new StorageException($"Cannot update {schema.TableName}: no index defined");

            var setClauses = new List<string>();
            var parameters = new List<SqlParameter>();
            int paramIdx = 0;

            foreach (var field in schema.Fields)
            {
                string pName = $"@s{paramIdx}";
                setClauses.Add($"{QuoteName(field.Name)} = {pName}");
                object? val = values.TryGetValue(field.Name, out var v) ? v : DBNull.Value;
                parameters.Add(new SqlParameter(pName, val ?? DBNull.Value));
                paramIdx++;
            }

            var whereParts = new List<string>();
            foreach (var seg in index.Segments)
            {
                var field = schema.Fields.FirstOrDefault(f => f.FieldNumber == seg.FieldNumber);
                if (field == null) continue;
                string pName = $"@w{paramIdx}";
                whereParts.Add($"{QuoteName(field.Name)} = {pName}");
                object? val = values.TryGetValue(field.Name, out var v) ? v : DBNull.Value;
                parameters.Add(new SqlParameter(pName, val ?? DBNull.Value));
                paramIdx++;
            }

            string sql = $"UPDATE {tableName} SET {string.Join(", ", setClauses)} WHERE {string.Join(" AND ", whereParts)}";
            using var cmd = new SqlCommand(sql, conn);
            foreach (var p in parameters)
                cmd.Parameters.Add(p);
            await cmd.ExecuteNonQueryAsync();
            handle.Buffer.IsDirty = false;
        }
    }

    /// <summary>
    /// Delete the current record.
    /// </summary>
    public async Task DeleteAsync(FileHandle handle)
    {
        if (!handle.HasRecord || handle.Buffer.IsNew)
            return;

        var conn = await GetConnectionAsync(handle.Schema.DatabaseSpace);
        var schema = handle.Schema;
        string tableName = QuoteName(schema.SchemaName) + "." + QuoteName(schema.TableName);

        var index = schema.Indexes.FirstOrDefault();
        if (index == null)
            throw new StorageException($"Cannot delete from {schema.TableName}: no index defined");

        var values = handle.Buffer.GetAll();
        var whereParts = new List<string>();
        var parameters = new List<SqlParameter>();
        int paramIdx = 0;

        foreach (var seg in index.Segments)
        {
            var field = schema.Fields.FirstOrDefault(f => f.FieldNumber == seg.FieldNumber);
            if (field == null) continue;
            string pName = $"@p{paramIdx++}";
            whereParts.Add($"{QuoteName(field.Name)} = {pName}");
            object? val = values.TryGetValue(field.Name, out var v) ? v : DBNull.Value;
            parameters.Add(new SqlParameter(pName, val ?? DBNull.Value));
        }

        string sql = $"DELETE FROM {tableName} WHERE {string.Join(" AND ", whereParts)}";
        using var cmd = new SqlCommand(sql, conn);
        foreach (var p in parameters)
            cmd.Parameters.Add(p);
        await cmd.ExecuteNonQueryAsync();

        handle.HasRecord = false;
        handle.Buffer.Clear(schema);
    }

    /// <summary>
    /// Clear the record buffer for a file.
    /// </summary>
    public void ClearBuffer(FileHandle handle)
    {
        handle.Buffer.Clear(handle.Schema);
        handle.HasRecord = false;
    }

    /// <summary>
    /// Open a cursor for SCAN iteration.
    /// </summary>
    public async Task<SqlDataReader> OpenScanCursorAsync(FileHandle handle, int indexNumber,
        string? startKey = null, string? scopeKey = null)
    {
        var conn = await GetConnectionAsync(handle.Schema.DatabaseSpace);
        var schema = handle.Schema;
        string tableName = QuoteName(schema.SchemaName) + "." + QuoteName(schema.TableName);

        string sql = $"SELECT * FROM {tableName}";
        var parameters = new List<SqlParameter>();

        var index = schema.Indexes.FirstOrDefault(i => i.IndexNumber == indexNumber);
        if (index != null)
        {
            var orderFields = index.Segments
                .Select(s => schema.Fields.FirstOrDefault(f => f.FieldNumber == s.FieldNumber))
                .Where(f => f != null)
                .Select(f => QuoteName(f!.Name))
                .ToList();

            if (startKey != null)
            {
                var (whereClause, whereParams) = BuildKeyWhere(schema, index, startKey, ">=");
                sql += $" WHERE {whereClause}";
                parameters.AddRange(whereParams);
            }

            if (orderFields.Count > 0)
                sql += $" ORDER BY {string.Join(", ", orderFields)}";
        }

        var cmd = new SqlCommand(sql, conn);
        foreach (var p in parameters)
            cmd.Parameters.Add(p);

        return await cmd.ExecuteReaderAsync();
    }

    private async Task EnsureTableExistsAsync(SqlConnection conn, TableSchema schema)
    {
        if (schema.Fields.Count == 0)
            return; // No fields defined, nothing to create

        string checkSql = @"
            SELECT COUNT(*) FROM INFORMATION_SCHEMA.TABLES
            WHERE TABLE_SCHEMA = @schema AND TABLE_NAME = @table";

        using (var cmd = new SqlCommand(checkSql, conn))
        {
            cmd.Parameters.AddWithValue("@schema", schema.SchemaName);
            cmd.Parameters.AddWithValue("@table", schema.TableName);
            int count = (int)(await cmd.ExecuteScalarAsync())!;
            if (count > 0)
                return;
        }

        var columnDefs = new List<string>();
        foreach (var field in schema.Fields)
        {
            string colType = field.NativeType switch
            {
                NativeType.String => $"NVARCHAR({field.NativeLength})",
                NativeType.Integer => field.NativeLength <= 2 ? "SMALLINT" : "INT",
                NativeType.Numeric => "FLOAT",
                NativeType.Date => "DATE",
                NativeType.Variable => "NVARCHAR(MAX)",
                NativeType.SmallInt => "SMALLINT",
                _ => $"NVARCHAR({field.NativeLength})",
            };

            string defaultClause = "";
            if (field.DefaultValue != null)
            {
                defaultClause = field.NativeType switch
                {
                    NativeType.String => $" DEFAULT '{field.DefaultValue.Replace("'", "''")}'",
                    NativeType.Integer or NativeType.SmallInt => $" DEFAULT {field.DefaultValue}",
                    NativeType.Numeric => $" DEFAULT {field.DefaultValue}",
                    NativeType.Date => $" DEFAULT '{field.DefaultValue}'",
                    _ => "",
                };
            }

            columnDefs.Add($"    {QuoteName(field.Name)} {colType} NULL{defaultClause}");
        }

        string createSql = $"CREATE TABLE {QuoteName(schema.SchemaName)}.{QuoteName(schema.TableName)} (\n{string.Join(",\n", columnDefs)}\n)";

        using (var cmd = new SqlCommand(createSql, conn))
        {
            await cmd.ExecuteNonQueryAsync();
        }

        foreach (var index in schema.Indexes)
        {
            var indexFields = index.Segments
                .Select(s => schema.Fields.FirstOrDefault(f => f.FieldNumber == s.FieldNumber))
                .Where(f => f != null)
                .Select(f => QuoteName(f!.Name))
                .ToList();

            if (indexFields.Count == 0) continue;

            string indexName = $"IX_{schema.TableName}_{index.IndexNumber}";
            string indexSql = $"CREATE INDEX {QuoteName(indexName)} ON {QuoteName(schema.SchemaName)}.{QuoteName(schema.TableName)} ({string.Join(", ", indexFields)})";

            using var cmd = new SqlCommand(indexSql, conn);
            await cmd.ExecuteNonQueryAsync();
        }
    }

    private static (string Where, List<SqlParameter> Params) BuildKeyWhere(
        TableSchema schema, IndexDefinition index, string keyValue, string op)
    {
        var firstField = schema.Fields.FirstOrDefault(f =>
            f.FieldNumber == index.Segments[0].FieldNumber);

        if (firstField == null)
            return ("1=1", []);

        string paramName = "@key0";
        object paramValue = firstField.NativeType switch
        {
            NativeType.Integer or NativeType.SmallInt =>
                int.TryParse(keyValue.Trim(), out var i) ? i : (object)keyValue.Trim(),
            NativeType.Numeric =>
                double.TryParse(keyValue.Trim(), out var d) ? d : (object)keyValue.Trim(),
            _ => keyValue,
        };

        return ($"{QuoteName(firstField.Name)} {op} {paramName}",
            [new SqlParameter(paramName, paramValue)]);
    }

    private static string QuoteName(string name) => $"[{name.Replace("]", "]]")}]";

    public void Dispose()
    {
        foreach (var conn in _connections.Values)
            conn.Dispose();
        _connections.Clear();
    }
}
