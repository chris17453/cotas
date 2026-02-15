namespace CoTAS.Storage;

/// <summary>
/// Represents an index segment (field + flags).
/// </summary>
public sealed class IndexSegment
{
    public required int FieldNumber { get; init; }
    public required int Flag { get; init; }
}

/// <summary>
/// Represents an index definition from a DDF .int file.
/// </summary>
public sealed class IndexDefinition
{
    public required int IndexNumber { get; init; }
    public List<IndexSegment> Segments { get; init; } = [];
}
