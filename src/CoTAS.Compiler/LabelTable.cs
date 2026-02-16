namespace CoTAS.Compiler;

/// <summary>
/// Collects labels during compilation and resolves forward references.
/// Labels are stored as 4-byte instruction byte offsets in the label segment.
/// </summary>
public sealed class LabelTable
{
    private readonly List<LabelEntry> _labels = [];
    private readonly Dictionary<string, int> _nameToIndex = new(StringComparer.OrdinalIgnoreCase);

    /// <summary>Number of labels.</summary>
    public int Count => _labels.Count;

    /// <summary>Number of labels imported from the original .RUN file.</summary>
    public int ImportedLabelCount { get; private set; }

    /// <summary>
    /// Register a label. Returns the label index.
    /// If the label was previously referenced (forward reference), resolves it.
    /// </summary>
    public int AddLabel(string name, int instructionByteOffset)
    {
        if (_nameToIndex.TryGetValue(name, out int existing))
        {
            // Forward reference — now resolve it
            _labels[existing] = _labels[existing] with { ByteOffset = instructionByteOffset, IsResolved = true };
            return existing;
        }

        int index = _labels.Count;
        _labels.Add(new LabelEntry(name, instructionByteOffset, true));
        _nameToIndex[name] = index;
        return index;
    }

    /// <summary>
    /// Get or create a label reference. If the label hasn't been defined yet,
    /// creates a forward reference that will be resolved later.
    /// Returns the label index (for use in GOTO/GOSUB spec bytes).
    /// </summary>
    public int GetOrCreateRef(string name)
    {
        if (_nameToIndex.TryGetValue(name, out int existing))
            return existing;

        // Forward reference — offset will be filled in later
        int index = _labels.Count;
        _labels.Add(new LabelEntry(name, 0, false));
        _nameToIndex[name] = index;
        return index;
    }

    /// <summary>
    /// Look up a label by name. Returns -1 if not found.
    /// </summary>
    public int FindLabel(string name)
    {
        return _nameToIndex.TryGetValue(name, out int idx) ? idx : -1;
    }

    /// <summary>
    /// Get the byte offset for a label.
    /// </summary>
    public int GetByteOffset(int index) => _labels[index].ByteOffset;

    /// <summary>
    /// Get the label name by index.
    /// </summary>
    public string GetName(int index) => _labels[index].Name;

    /// <summary>
    /// Check if all labels are resolved.
    /// </summary>
    public bool AllResolved => _labels.All(l => l.IsResolved);

    /// <summary>
    /// Get unresolved label names (for error reporting).
    /// </summary>
    public IEnumerable<string> GetUnresolved() =>
        _labels.Where(l => !l.IsResolved).Select(l => l.Name);

    /// <summary>
    /// Get all label byte offsets in order (for the label segment).
    /// </summary>
    public List<int> GetAllOffsets() => _labels.Select(l => l.ByteOffset).ToList();

    /// <summary>
    /// Import labels from a RunFileReader (for round-trip).
    /// </summary>
    public void ImportOffsets(List<int> offsets, int headerNumLabels = 0)
    {
        // Import offsets from the label segment
        for (int i = 0; i < offsets.Count; i++)
        {
            string name = $"LABEL_{i}";
            _labels.Add(new LabelEntry(name, offsets[i], true));
            _nameToIndex[name] = i;
        }
        // Pad to header's NumLabels count (some labels may exist beyond segment entries)
        int totalLabels = Math.Max(offsets.Count, headerNumLabels);
        for (int i = offsets.Count; i < totalLabels; i++)
        {
            string name = $"LABEL_{i}";
            _labels.Add(new LabelEntry(name, 0, true));
            _nameToIndex[name] = i;
        }
        ImportedLabelCount = _labels.Count;
    }

    /// <summary>
    /// Set label byte offset by index (for round-trip with original data).
    /// Extends the label list as needed.
    /// </summary>
    public void SetLabelOffset(int index, int byteOffset)
    {
        while (_labels.Count <= index)
        {
            string name = $"LABEL_{_labels.Count}";
            _labels.Add(new LabelEntry(name, 0, false));
            _nameToIndex[name] = _labels.Count - 1;
        }
        _labels[index] = _labels[index] with { ByteOffset = byteOffset, IsResolved = true };
    }

    private record LabelEntry(string Name, int ByteOffset, bool IsResolved);
}
