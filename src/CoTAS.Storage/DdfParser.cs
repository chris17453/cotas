namespace CoTAS.Storage;

/// <summary>
/// Parses DDF .int files into TableSchema objects.
/// Caches parsed schemas by file path.
/// </summary>
public sealed class DdfParser
{
    private readonly Dictionary<string, TableSchema> _cache = new(StringComparer.OrdinalIgnoreCase);

    /// <summary>
    /// Parse a .int file and return its TableSchema. Results are cached.
    /// </summary>
    public TableSchema Parse(string intFilePath)
    {
        string key = Path.GetFullPath(intFilePath);
        if (_cache.TryGetValue(key, out var cached))
            return cached;

        string text = File.ReadAllText(intFilePath);
        var schema = ParseText(text);
        _cache[key] = schema;
        return schema;
    }

    /// <summary>
    /// Parse DDF text content into a TableSchema.
    /// </summary>
    public static TableSchema ParseText(string text)
    {
        var lines = text.Split('\n')
            .Select(l => l.Trim())
            .Where(l => l.Length > 0)
            .ToList();

        string databaseSpace = "";
        string tableName = "";
        string schemaName = "dbo";
        int numFields = 0;
        int pageSize = 4096;
        int recordLength = 0;

        var fields = new List<FieldDefinition>();
        var indexes = new List<IndexDefinition>();

        // Current field being parsed
        int? currentFieldNumber = null;
        string? currentFieldName = null;
        int currentNativeType = 0;
        int currentNativeLength = 0;
        int currentNativeOffset = 0;
        string? currentDefaultValue = null;
        int? currentFieldIndex = null;

        // Current index being parsed
        int? currentIndexNumber = null;
        var currentSegments = new List<IndexSegment>();

        void FlushField()
        {
            if (currentFieldNumber != null && currentFieldName != null)
            {
                // Clean array notation from field name: "FIELD[  1]" → "FIELD[1]"
                string cleanName = currentFieldName.Replace(" ", "");

                fields.Add(new FieldDefinition
                {
                    FieldNumber = currentFieldNumber.Value,
                    Name = cleanName,
                    NativeType = (NativeType)currentNativeType,
                    NativeLength = currentNativeLength,
                    NativeOffset = currentNativeOffset,
                    DefaultValue = currentDefaultValue,
                    IndexRef = currentFieldIndex,
                });
            }
            currentFieldNumber = null;
            currentFieldName = null;
            currentNativeType = 0;
            currentNativeLength = 0;
            currentNativeOffset = 0;
            currentDefaultValue = null;
            currentFieldIndex = null;
        }

        void FlushIndex()
        {
            if (currentIndexNumber != null && currentSegments.Count > 0)
            {
                // Remove terminator segment (field 0, flag -1)
                var realSegments = currentSegments
                    .Where(s => s.FieldNumber > 0)
                    .ToList();

                indexes.Add(new IndexDefinition
                {
                    IndexNumber = currentIndexNumber.Value,
                    Segments = realSegments,
                });
            }
            currentIndexNumber = null;
            currentSegments = [];
        }

        // Temporary for building index segments
        int? pendingSegmentField = null;

        foreach (var line in lines)
        {
            int spaceIdx = line.IndexOf(' ');
            if (spaceIdx < 0) continue;

            string key = line[..spaceIdx];
            string value = line[(spaceIdx + 1)..].Trim();

            switch (key)
            {
                case "DATABASE_SPACE_NAME":
                    databaseSpace = value;
                    break;
                case "TABLE_NAME":
                    tableName = value;
                    break;
                case "SCHEMA_NAME":
                    schemaName = value;
                    break;
                case "NUMBER_DF_FIELDS":
                    numFields = int.Parse(value);
                    break;
                case "PAGE_SIZE":
                    pageSize = int.Parse(value);
                    break;
                case "LOGICAL_RECORD_LENGTH":
                    recordLength = int.Parse(value);
                    break;

                // Field parsing
                case "FIELD_NUMBER":
                    FlushField();
                    currentFieldNumber = int.Parse(value);
                    break;
                case "FIELD_NAME":
                    currentFieldName = value;
                    break;
                case "FIELD_NATIVE_TYPE":
                    currentNativeType = int.Parse(value);
                    break;
                case "FIELD_NATIVE_LENGTH":
                    currentNativeLength = int.Parse(value);
                    break;
                case "FIELD_NATIVE_OFFSET":
                    currentNativeOffset = int.Parse(value);
                    break;
                case "FIELD_DEFAULT_VALUE":
                    currentDefaultValue = value;
                    break;
                case "FIELD_INDEX":
                    currentFieldIndex = int.Parse(value);
                    break;

                // Index parsing
                case "INDEX_NUMBER":
                    FlushField(); // ensure last field is flushed before indexes
                    FlushIndex();
                    currentIndexNumber = int.Parse(value);
                    break;
                case "INDEX_SEGMENT_FIELD":
                    if (pendingSegmentField != null)
                    {
                        // Previous segment had no flag yet — shouldn't happen, but handle gracefully
                    }
                    pendingSegmentField = int.Parse(value);
                    break;
                case "INDEX_SEGMENT_FLAG":
                    if (pendingSegmentField != null)
                    {
                        currentSegments.Add(new IndexSegment
                        {
                            FieldNumber = pendingSegmentField.Value,
                            Flag = int.Parse(value),
                        });
                        pendingSegmentField = null;
                    }
                    break;

                // Ignored keys
                case "PERMANENT_INT":
                case "LOCAL_CACHE":
                case "FILE_FLAGS":
                case "FILE_DDF_FLAGS":
                case "IGNORE_NULL_VALUES":
                case "TRIM_STRING_FIELDS":
                case "INDEX_NUMBER_SEGMENTS":
                case "INDEX_SEGMENT_NULL_VALUE":
                case "FIELD_NATIVE_FLAG":
                case "FIELD_TYPE":
                case "FIELD_LENGTH":
                    break;
            }
        }

        FlushField();
        FlushIndex();

        return new TableSchema
        {
            DatabaseSpace = databaseSpace,
            TableName = tableName,
            SchemaName = schemaName,
            NumberOfFields = numFields,
            PageSize = pageSize,
            LogicalRecordLength = recordLength,
            Fields = fields,
            Indexes = indexes,
        };
    }

    /// <summary>
    /// Find the .int file for a given TAS file name within a directory.
    /// TAS uses short names (e.g., "BKACMOD") which map to files like "bkacmod_.int".
    /// </summary>
    public static string? FindIntFile(string ddfDirectory, string tasFileName)
    {
        if (!Directory.Exists(ddfDirectory))
            return null;

        // Try exact match first
        string exact = Path.Combine(ddfDirectory, tasFileName.ToLower() + ".int");
        if (File.Exists(exact))
            return exact;

        // Try with underscore suffix (common in GPACIFIC)
        string withUnderscore = Path.Combine(ddfDirectory, tasFileName.ToLower() + "_.int");
        if (File.Exists(withUnderscore))
            return withUnderscore;

        // Search all .int files for matching TABLE_NAME
        foreach (var file in Directory.GetFiles(ddfDirectory, "*.int"))
        {
            try
            {
                // Quick scan first few lines for TABLE_NAME
                foreach (var line in File.ReadLines(file).Take(5))
                {
                    if (line.StartsWith("TABLE_NAME ", StringComparison.OrdinalIgnoreCase))
                    {
                        string name = line["TABLE_NAME ".Length..].Trim();
                        if (name.Equals(tasFileName, StringComparison.OrdinalIgnoreCase))
                            return file;
                        break;
                    }
                }
            }
            catch { /* skip unreadable files */ }
        }

        return null;
    }
}
