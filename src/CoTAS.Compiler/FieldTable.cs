using CoTAS.Parser.RunFile;

namespace CoTAS.Compiler;

/// <summary>
/// Manages field definitions during compilation.
/// Assigns byte offsets and builds the field spec segment.
/// </summary>
public sealed class FieldTable
{
    private readonly List<FieldEntry> _fields = [];
    private readonly Dictionary<string, int> _nameToIndex = new(StringComparer.OrdinalIgnoreCase);
    private int _fieldSpecSize = RunFileHeader.OldFieldSpecSize; // 48 bytes for TAS32
    private int _nextDefinedOffset;
    private int _nextTempOffset;
    private int _tempFieldCount;
    private int _screenFieldCount;
    private int _definedDataSize;
    private int _overlayFieldCount;

    /// <summary>Number of fields defined.</summary>
    public int Count => _fields.Count;

    /// <summary>Number of temp fields.</summary>
    public int TempFieldCount => _tempFieldCount;

    /// <summary>Screen field count.</summary>
    public int ScreenFieldCount { get => _screenFieldCount; set => _screenFieldCount = value; }

    /// <summary>Defined field data segment size.</summary>
    public int DefinedDataSize => _definedDataSize;

    /// <summary>Temp field data area end offset.</summary>
    public int TempFieldAreaEnd => _nextTempOffset;

    /// <summary>Temp field data area size.</summary>
    public int TempFieldSize => _nextTempOffset;

    /// <summary>Field spec size (48 or 60).</summary>
    public int FieldSpecSize { get => _fieldSpecSize; set => _fieldSpecSize = value; }

    /// <summary>
    /// Add a defined field. Returns the field index.
    /// </summary>
    public int AddDefinedField(string name, char fieldType, int displaySize, int decimals = 0,
        int arrayCount = 0, bool reset = false, int internalSize = 0)
    {
        if (_nameToIndex.TryGetValue(name, out int existing))
            return existing;

        int index = _fields.Count;

        // Calculate internal size if not specified
        if (internalSize == 0)
            internalSize = CalculateInternalSize(fieldType, displaySize, decimals, arrayCount);

        int dataOffset = _nextDefinedOffset;
        _nextDefinedOffset += internalSize * Math.Max(1, arrayCount);

        var spec = new RunFieldSpec
        {
            Name = name,
            Offset = dataOffset,
            FieldType = fieldType,
            Decimals = decimals,
            DisplaySize = displaySize,
            ArrayCount = arrayCount,
            IsReset = reset,
            InternalSize = internalSize,
        };

        _fields.Add(new FieldEntry(spec, false));
        _nameToIndex[name] = index;
        _definedDataSize = _nextDefinedOffset;
        return index;
    }

    /// <summary>
    /// Add a temp field. Returns the field index.
    /// </summary>
    public int AddTempField(string name, char fieldType, int displaySize, int decimals = 0, int internalSize = 0)
    {
        if (_nameToIndex.TryGetValue(name, out int existing))
            return existing;

        int index = _fields.Count;

        if (internalSize == 0)
            internalSize = CalculateInternalSize(fieldType, displaySize, decimals, 0);

        int dataOffset = _nextTempOffset;
        _nextTempOffset += internalSize;
        _tempFieldCount++;

        var spec = new RunFieldSpec
        {
            Name = name,
            Offset = dataOffset,
            FieldType = fieldType,
            Decimals = decimals,
            DisplaySize = displaySize,
            InternalSize = internalSize,
        };

        _fields.Add(new FieldEntry(spec, true));
        _nameToIndex[name] = index;
        return index;
    }

    /// <summary>
    /// Add a file field (from OPEN command). Returns the field index.
    /// </summary>
    public int AddFileField(string name, char fieldType, int displaySize, int decimals,
        int internalSize, byte keyNumber, byte bufferNumber, ushort fileHandle,
        int arrayCount = 0, char pictureType = '\0', int pictureLocation = 0)
    {
        if (_nameToIndex.TryGetValue(name, out int existing))
            return existing;

        int index = _fields.Count;
        int dataOffset = _nextDefinedOffset;
        _nextDefinedOffset += internalSize * Math.Max(1, arrayCount);

        var spec = new RunFieldSpec
        {
            Name = name,
            Offset = dataOffset,
            FieldType = fieldType,
            Decimals = decimals,
            DisplaySize = displaySize,
            ArrayCount = arrayCount,
            IsFileField = true,
            KeyNumber = keyNumber,
            FileBufferNumber = bufferNumber,
            FileHandle = fileHandle,
            InternalSize = internalSize,
            PictureType = pictureType,
            PictureLocation = pictureLocation,
        };

        _fields.Add(new FieldEntry(spec, false));
        _nameToIndex[name] = index;
        _definedDataSize = _nextDefinedOffset;
        return index;
    }

    /// <summary>
    /// Look up a field by name. Returns -1 if not found.
    /// </summary>
    public int FindField(string name)
    {
        return _nameToIndex.TryGetValue(name, out int idx) ? idx : -1;
    }

    /// <summary>
    /// Get field spec by index.
    /// </summary>
    public RunFieldSpec GetField(int index) => _fields[index].Spec;

    /// <summary>
    /// Get the byte offset for a field index (for use in spec params).
    /// This is fieldIndex * fieldSpecSize.
    /// </summary>
    public int GetFieldSpecOffset(int index) => index * _fieldSpecSize;

    /// <summary>
    /// Get the byte offset for a field by name.
    /// </summary>
    public int GetFieldSpecOffset(string name)
    {
        int idx = FindField(name);
        if (idx < 0)
            throw new InvalidOperationException($"Field not found: {name}");
        return idx * _fieldSpecSize;
    }

    /// <summary>
    /// Get all field specs in order.
    /// </summary>
    public List<RunFieldSpec> GetAllSpecs() => _fields.Select(f => f.Spec).ToList();

    /// <summary>Get only the local field specs (excluding overlay fields) for output.</summary>
    public List<RunFieldSpec> GetLocalSpecs() => _fields.Skip(_overlayFieldCount).Select(f => f.Spec).ToList();

    /// <summary>
    /// Import fields from a RunFileReader (for round-trip).
    /// </summary>
    /// <summary>Number of overlay fields (excluded from output but used for name resolution).</summary>
    public int OverlayFieldCount => _overlayFieldCount;

    public void ImportFromReader(RunFileReader run)
    {
        _fieldSpecSize = run.Header.FieldSpecSize;
        _overlayFieldCount = run.OverlayFieldCount;
        foreach (var f in run.Fields)
        {
            int index = _fields.Count;
            _fields.Add(new FieldEntry(f, f.IsTempField));
            _nameToIndex[f.Name] = index;
            if (index >= _overlayFieldCount && f.IsTempField)
                _tempFieldCount++;
        }
        _nextDefinedOffset = run.Header.DefFldSegSize;
        _nextTempOffset = run.Header.TempFlds;
        _definedDataSize = run.Header.DefFldSegSize;
        _screenFieldCount = run.Header.ScrnFldNum;
    }

    private static int CalculateInternalSize(char fieldType, int displaySize, int decimals, int arrayCount)
    {
        return fieldType switch
        {
            'A' => displaySize,
            'I' => 4,
            'N' => displaySize + 1, // sign + digits + decimal point
            'D' => 8,
            'T' => 6,
            'L' => 1,
            'R' => 8,
            'B' => 1,
            'F' => 8,
            'P' => (displaySize + 1) / 2,
            _ => displaySize
        };
    }

    private record FieldEntry(RunFieldSpec Spec, bool IsTemp);
}
