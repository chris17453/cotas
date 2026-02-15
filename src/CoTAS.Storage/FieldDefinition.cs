namespace CoTAS.Storage;

/// <summary>
/// DDF native type codes as found in .int files.
/// </summary>
public enum NativeType
{
    String = 0,
    Integer = 1,
    Numeric = 2,    // 8-byte double
    Date = 3,       // 4-byte date
    Variable = 13,  // Variable-length blob
    SmallInt = 14,  // 2-byte integer
}

/// <summary>
/// Represents a field definition from a DDF .int file.
/// </summary>
public sealed class FieldDefinition
{
    public required int FieldNumber { get; init; }
    public required string Name { get; init; }
    public required NativeType NativeType { get; init; }
    public required int NativeLength { get; init; }
    public required int NativeOffset { get; init; }
    public string? DefaultValue { get; init; }
    public int? IndexRef { get; init; }

    /// <summary>
    /// Get the SQLite column type for this field.
    /// </summary>
    public string SqliteType => NativeType switch
    {
        NativeType.String => "TEXT",
        NativeType.Integer => "INTEGER",
        NativeType.Numeric => "REAL",
        NativeType.Date => "TEXT",       // stored as YYYYMMDD
        NativeType.Variable => "TEXT",
        NativeType.SmallInt => "INTEGER",
        _ => "TEXT",
    };

    /// <summary>
    /// Get the TAS type code for FieldManager registration.
    /// </summary>
    public string TasTypeCode => NativeType switch
    {
        NativeType.String => "A",
        NativeType.Integer => "I",
        NativeType.Numeric => "N",
        NativeType.Date => "D",
        NativeType.Variable => "A",
        NativeType.SmallInt => "I",
        _ => "A",
    };
}
