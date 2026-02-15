namespace CoTAS.Parser.RunFile;

/// <summary>
/// A file buffer entry from the .RUN file buffer list.
/// 16 bytes each, 100 entries. Maps to TBufferList.
/// </summary>
public sealed class RunBufferEntry
{
    /// <summary>Buffer name (8 characters, e.g. "BKARCUST").</summary>
    public string Name { get; set; } = "";

    /// <summary>Buffer pointer (runtime, not meaningful in file).</summary>
    public int BufferPtr { get; set; }

    /// <summary>File handle number.</summary>
    public int FileHandle { get; set; }

    /// <summary>Whether this entry is used (non-empty name).</summary>
    public bool IsUsed => !string.IsNullOrWhiteSpace(Name);

    public override string ToString() => $"{Name} (handle={FileHandle})";
}
