namespace CoTAS.Storage;

/// <summary>
/// In-memory buffer for a single record. Maps field names to values.
/// Field names are stored with the table prefix (e.g., "BKAR.CUSTNAME").
/// </summary>
public sealed class RecordBuffer
{
    private readonly Dictionary<string, object?> _values = new(StringComparer.OrdinalIgnoreCase);

    public bool IsNew { get; set; } = true;
    public bool IsDirty { get; set; }

    public object? Get(string fieldName)
    {
        return _values.TryGetValue(fieldName, out var val) ? val : null;
    }

    public void Set(string fieldName, object? value)
    {
        _values[fieldName] = value;
        IsDirty = true;
    }

    public void Clear(TableSchema schema)
    {
        _values.Clear();
        IsNew = true;
        IsDirty = false;

        // Initialize with defaults
        foreach (var field in schema.Fields)
        {
            _values[field.Name] = field.NativeType switch
            {
                NativeType.String => new string(' ', field.NativeLength),
                NativeType.Integer or NativeType.SmallInt => 0,
                NativeType.Numeric => 0.0,
                NativeType.Date => "",
                NativeType.Variable => "",
                _ => "",
            };
        }
    }

    /// <summary>
    /// Load values from a SqlDataReader row.
    /// </summary>
    public void LoadFromReader(Microsoft.Data.SqlClient.SqlDataReader reader, TableSchema schema)
    {
        _values.Clear();
        IsNew = false;
        IsDirty = false;

        for (int i = 0; i < reader.FieldCount; i++)
        {
            string colName = reader.GetName(i);
            object val = reader.IsDBNull(i) ? GetDefault(schema, colName) : reader.GetValue(i);
            _values[colName] = val;
        }
    }

    /// <summary>
    /// Get all field name/value pairs for SQL operations.
    /// </summary>
    public IReadOnlyDictionary<string, object?> GetAll() => _values;

    private static object GetDefault(TableSchema schema, string colName)
    {
        var field = schema.Fields.FirstOrDefault(f =>
            f.Name.Equals(colName, StringComparison.OrdinalIgnoreCase));

        if (field == null) return "";

        return field.NativeType switch
        {
            NativeType.String => "",
            NativeType.Integer or NativeType.SmallInt => 0,
            NativeType.Numeric => 0.0,
            NativeType.Date => "",
            NativeType.Variable => "",
            _ => "",
        };
    }
}
