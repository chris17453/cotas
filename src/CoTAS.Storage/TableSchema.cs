namespace CoTAS.Storage;

/// <summary>
/// Represents a parsed DDF table schema from a .int file.
/// </summary>
public sealed record TableSchema
{
    public required string DatabaseSpace { get; init; }
    public required string TableName { get; init; }
    public string SchemaName { get; init; } = "dbo";
    public int NumberOfFields { get; init; }
    public int PageSize { get; init; } = 4096;
    public int LogicalRecordLength { get; init; }
    public List<FieldDefinition> Fields { get; init; } = [];
    public List<IndexDefinition> Indexes { get; init; } = [];

    /// <summary>
    /// Get the TAS file name used in OPENV (e.g., "BKACMOD" from table BKACMOD).
    /// </summary>
    public string TasFileName => TableName;
}
