namespace CoTAS.Storage;

/// <summary>
/// Runtime state for an open TAS file. Tracks schema, current record, and cursor state.
/// </summary>
public sealed class FileHandle
{
    public required int FileNumber { get; init; }
    public required TableSchema Schema { get; init; }
    public required string TasFileName { get; init; }

    /// <summary>Current record buffer.</summary>
    public RecordBuffer Buffer { get; } = new();

    /// <summary>Whether a record is currently loaded.</summary>
    public bool HasRecord { get; set; }

    /// <summary>Whether the file has reached EOF.</summary>
    public bool IsEof { get; set; }

    /// <summary>Whether the file has reached BOF.</summary>
    public bool IsBof { get; set; }

    /// <summary>Last error code from file operations (0 = no error).</summary>
    public int LastError { get; set; }

    /// <summary>Current record number/identity value.</summary>
    public long CurrentRecordId { get; set; }

    /// <summary>Lock mode: "NONE", "LOCK", "NLOCK".</summary>
    public string LockMode { get; set; } = "NONE";

    /// <summary>
    /// Get the qualified field name as TAS sees it: "TABLENAME.FIELDNAME" or just "FIELDNAME".
    /// The TAS convention is to prefix with the table name for file fields.
    /// </summary>
    public string QualifyFieldName(string rawFieldName)
    {
        // If already qualified, return as-is
        if (rawFieldName.Contains('.'))
            return rawFieldName;
        return $"{TasFileName}.{rawFieldName}";
    }
}
