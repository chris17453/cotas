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
        int index = EnsureLabelSlot(name);
        _labels[index] = _labels[index] with { ByteOffset = instructionByteOffset, IsResolved = true };
        return index;
    }

    /// <summary>
    /// Get or create a label reference. If the label hasn't been defined yet,
    /// creates a forward reference that will be resolved later.
    /// Returns the label index (for use in GOTO/GOSUB spec bytes).
    /// </summary>
    public int GetOrCreateRef(string name)
    {
        return EnsureLabelSlot(name);
    }

    /// <summary>
    /// Ensures a label slot exists at the correct index.
    /// For LABEL_N names, forces index = N to match TAS convention.
    /// </summary>
    private int EnsureLabelSlot(string name)
    {
        if (_nameToIndex.TryGetValue(name, out int existing))
            return existing;

        // For LABEL_N names, use N as the index to match TAS numbering
        int targetIndex;
        if (name.StartsWith("LABEL_", StringComparison.OrdinalIgnoreCase)
            && int.TryParse(name.AsSpan(6), out int labelNum))
        {
            targetIndex = labelNum;
        }
        else
        {
            targetIndex = _labels.Count;
        }

        // Extend list to accommodate the target index
        while (_labels.Count <= targetIndex)
        {
            int padIdx = _labels.Count;
            string padName = $"LABEL_{padIdx}";
            _labels.Add(new LabelEntry(padName, 0, false));
            if (!_nameToIndex.ContainsKey(padName))
                _nameToIndex[padName] = padIdx;
        }

        _nameToIndex[name] = targetIndex;
        return targetIndex;
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
    /// Only includes entries up to the highest defined (resolved) label index.
    /// </summary>
    public List<int> GetAllOffsets()
    {
        // Find the highest resolved label index
        int maxResolved = -1;
        for (int i = 0; i < _labels.Count; i++)
        {
            if (_labels[i].IsResolved)
                maxResolved = i;
        }

        var offsets = new List<int>();
        for (int i = 0; i <= maxResolved; i++)
            offsets.Add(_labels[i].ByteOffset);
        return offsets;
    }

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
