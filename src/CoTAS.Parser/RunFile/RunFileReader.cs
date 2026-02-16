using System.Text;

namespace CoTAS.Parser.RunFile;

/// <summary>
/// Reads and parses TAS compiled .RUN files.
/// Supports TAS 5.1 (DOS) format with "TAS32" signature.
///
/// File layout:
///   [128 bytes]    TPci header
///   [1600 bytes]   File buffer list (100 × 16 bytes)
///   [CodeSize]     Bytecode instructions (8 bytes each)
///   [ConstSize]    Constant segment (string literals, etc.)
///   [SpecSize]     Spec segment (instruction parameters)
///   [LabelSize]    Label offsets (4 bytes each)
///   [FldNameSize]  Field specifications (48 bytes each)
/// </summary>
public sealed class RunFileReader
{
    private readonly byte[] _data;
    private readonly string _filePath;

    public RunFileHeader Header { get; }
    public List<RunBufferEntry> Buffers { get; } = [];
    public List<RunFieldSpec> Fields { get; } = [];
    public List<int> LabelOffsets { get; } = [];
    public List<RunBytecodeInstruction> Instructions { get; } = [];

    /// <summary>Raw header bytes (128 bytes, for round-trip fidelity).</summary>
    public byte[] RawHeader { get; }

    /// <summary>Raw buffer list bytes (1600 bytes, for round-trip fidelity).</summary>
    public byte[] RawBufferList { get; }

    /// <summary>Raw constant segment bytes.</summary>
    public byte[] ConstantSegment { get; private set; }

    /// <summary>Raw spec segment bytes.</summary>
    public byte[] SpecSegment { get; private set; }

    /// <summary>Raw code segment bytes.</summary>
    public byte[] CodeSegment { get; }

    /// <summary>Raw label segment bytes.</summary>
    public byte[] LabelSegment { get; }

    /// <summary>Raw field spec segment bytes.</summary>
    public byte[] FieldSpecSegment { get; }

    private RunFileReader(byte[] data, string filePath)
    {
        _data = data;
        _filePath = filePath;
        Header = ReadHeader();
        RawHeader = ReadSegment(0, RunFileHeader.Size);
        RawBufferList = ReadSegment(RunFileHeader.Size, RunFileHeader.BufferListSize);
        CodeSegment = ReadSegment(Header.CodeOffset, Header.CodeSize);
        ConstantSegment = ReadSegment(Header.ConstOffset, Header.ConstSize);
        SpecSegment = ReadSegment(Header.SpecOffset, Header.SpecSize);
        LabelSegment = ReadSegment(Header.LabelOffset, Header.LabelSize);
        FieldSpecSegment = ReadSegment(Header.FieldSpecOffset, Header.FldNameSize);
        ReadBuffers();
        ReadInstructions();
        ReadLabels();
        ReadFields();
    }

    /// <summary>
    /// Load and parse a .RUN file.
    /// </summary>
    public static RunFileReader Load(string filePath)
    {
        byte[] data = File.ReadAllBytes(filePath);
        if (data.Length < RunFileHeader.Size)
            throw new InvalidDataException($"File too small to be a .RUN file: {data.Length} bytes");
        return new RunFileReader(data, filePath);
    }

    /// <summary>
    /// Load a .RUN file with an overlay (.OVL) prepended.
    /// TAS programs reference shared overlay data: the overlay's spec, constant,
    /// field, and label segments are prepended before the program's own segments.
    /// SLPtr values in the program then index into the combined spec segment.
    /// </summary>
    public static RunFileReader LoadWithOverlay(string filePath, string overlayPath)
    {
        var run = Load(filePath);
        var ovl = Load(overlayPath);
        run.PrependOverlay(ovl);
        return run;
    }

    /// <summary>
    /// Auto-detect and load overlay if needed. Looks for ADV50.OVL in same directory.
    /// </summary>
    public static RunFileReader LoadAutoOverlay(string filePath)
    {
        var run = Load(filePath);
        // Check if overlay is needed: spec references beyond our segment,
        // OR DefFldSegSize implies more fields than we have (overlay fields)
        bool needsOverlay = false;
        foreach (var instr in run.Instructions)
        {
            if (instr.SpecLineSize > 0 && instr.SpecLinePtr + instr.SpecLineSize > run.SpecSegment.Length)
            {
                needsOverlay = true;
                break;
            }
        }
        if (!needsOverlay && run.Header.DefFldSegSize > run.Header.NumFlds * run.Header.FieldSpecSize)
        {
            needsOverlay = true; // Field references extend beyond our own fields
        }

        if (needsOverlay)
        {
            string dir = Path.GetDirectoryName(Path.GetFullPath(filePath)) ?? ".";
            string ovlPath = Path.Combine(dir, "ADV50.OVL");
            if (File.Exists(ovlPath))
            {
                var ovl = Load(ovlPath);
                run.PrependOverlay(ovl);
            }
        }

        return run;
    }

    /// <summary>
    /// Prepend overlay segments to this reader's segments.
    /// After this call, SpecSegment = ovl.Spec + this.Spec, etc.
    /// Field indices and constant offsets in existing spec data that reference
    /// the program's own data remain correct because the overlay is PREPENDED.
    /// </summary>
    private void PrependOverlay(RunFileReader ovl)
    {
        OverlaySpecSize = ovl.SpecSegment.Length;
        OverlayConstSize = ovl.ConstantSegment.Length;
        OverlayFieldCount = ovl.Fields.Count;
        OverlayLabelCount = ovl.LabelOffsets.Count;

        SpecSegment = CombineArrays(ovl.SpecSegment, SpecSegment);
        ConstantSegment = CombineArrays(ovl.ConstantSegment, ConstantSegment);

        // Prepend overlay fields
        var combinedFields = new List<RunFieldSpec>(ovl.Fields.Count + Fields.Count);
        combinedFields.AddRange(ovl.Fields);
        combinedFields.AddRange(Fields);
        Fields.Clear();
        Fields.AddRange(combinedFields);

        // Prepend overlay labels
        var combinedLabels = new List<int>(ovl.LabelOffsets.Count + LabelOffsets.Count);
        combinedLabels.AddRange(ovl.LabelOffsets);
        combinedLabels.AddRange(LabelOffsets);
        LabelOffsets.Clear();
        LabelOffsets.AddRange(combinedLabels);
    }

    private static byte[] CombineArrays(byte[] a, byte[] b)
    {
        byte[] result = new byte[a.Length + b.Length];
        Array.Copy(a, 0, result, 0, a.Length);
        Array.Copy(b, 0, result, a.Length, b.Length);
        return result;
    }

    /// <summary>Size of the overlay spec segment (0 if no overlay loaded).</summary>
    public int OverlaySpecSize { get; private set; }

    /// <summary>Size of the overlay constant segment (0 if no overlay loaded).</summary>
    public int OverlayConstSize { get; private set; }

    /// <summary>Number of fields from the overlay (0 if no overlay loaded).</summary>
    public int OverlayFieldCount { get; private set; }

    /// <summary>Number of labels from the overlay (0 if no overlay loaded).</summary>
    public int OverlayLabelCount { get; private set; }

    /// <summary>
    /// Get a string from the constant segment at the given offset.
    /// Reads until null terminator or end of segment.
    /// </summary>
    public string GetConstantString(int offset, int maxLength = 256)
    {
        if (offset < 0 || offset >= ConstantSegment.Length)
            return "";
        int end = offset;
        while (end < ConstantSegment.Length && end - offset < maxLength && ConstantSegment[end] != 0)
            end++;
        return Encoding.ASCII.GetString(ConstantSegment, offset, end - offset);
    }

    /// <summary>
    /// Get non-temp, non-file defined fields (user DEFINE'd fields).
    /// </summary>
    public IEnumerable<RunFieldSpec> GetDefinedFields() =>
        Fields.Where(f => !f.IsTempField && !f.IsFileField);

    /// <summary>
    /// Get file-related fields (from data dictionary / OPEN commands).
    /// </summary>
    public IEnumerable<RunFieldSpec> GetFileFields() =>
        Fields.Where(f => f.IsFileField);

    /// <summary>
    /// Get fields that were inherited from parent program (RESET fields from CHAIN).
    /// </summary>
    public IEnumerable<RunFieldSpec> GetResetFields() =>
        Fields.Where(f => f.IsReset);

    private RunFileHeader ReadHeader()
    {
        var h = new RunFileHeader
        {
            CodeSize = ReadInt32(0),
            ConstSize = ReadInt32(4),
            SpecSize = ReadInt32(8),
            LabelSize = ReadInt32(12),
            ScrnFldNum = ReadInt32(16),
            NumFlds = ReadInt32(20),
            TempFlds = ReadInt32(24),
            NumTempFlds = ReadInt32(28),
            FldNameSize = ReadInt32(32),
            TempFldSize = ReadInt32(36),
            DefFldSegSize = ReadInt32(40),
            NumExtraFlds = ReadInt32(44),
            PrgNames = ReadInt32(48),
            DebugFlg = _data[52] != 0,
            ProType = Encoding.ASCII.GetString(_data, 53, 5).TrimEnd('\0'),
            NumLabels = ReadInt32(58),
            NewFldSpec = _data[62] != 0,
            ChkUpVld = _data[63] != 0,
            IncLabels = _data[64] != 0,
        };

        if (h.ProType != "TAS32" && h.ProType != "TASWN")
            throw new InvalidDataException(
                $"Invalid .RUN file signature: expected 'TAS32' or 'TASWN', got '{h.ProType}'");

        return h;
    }

    private void ReadBuffers()
    {
        int offset = RunFileHeader.Size; // 128
        for (int i = 0; i < 100; i++)
        {
            string name = ReadFixedString(offset, 8);
            var entry = new RunBufferEntry
            {
                Name = name,
                BufferPtr = ReadInt32(offset + 8),
                FileHandle = ReadInt32(offset + 12),
            };
            if (entry.IsUsed)
                Buffers.Add(entry);
            offset += 16;
        }
    }

    private void ReadInstructions()
    {
        int offset = Header.CodeOffset;
        bool isTas51 = Header.ProType == "TAS32";
        int instrSize = isTas51 ? RunFileHeader.Tas51InstructionSize : RunFileHeader.Tas60InstructionSize;
        int count = Header.CodeSize / instrSize;

        for (int i = 0; i < count; i++)
        {
            if (isTas51)
            {
                // TAS 5.1: word CmdNum (2) + byte SLSize (1) + int32 SLPtr (4) = 7 bytes
                Instructions.Add(new RunBytecodeInstruction
                {
                    CommandNumber = ReadUInt16(offset),
                    Exit = 0,
                    SpecLineSize = _data[offset + 2],
                    SpecLinePtr = ReadInt32(offset + 3),
                });
            }
            else
            {
                // TAS 6.0: word CmdNum (2) + byte Exit (1) + byte SLSize (1) + int32 SLPtr (4) = 8 bytes
                Instructions.Add(new RunBytecodeInstruction
                {
                    CommandNumber = ReadUInt16(offset),
                    Exit = _data[offset + 2],
                    SpecLineSize = _data[offset + 3],
                    SpecLinePtr = ReadInt32(offset + 4),
                });
            }
            offset += instrSize;
        }
    }

    private void ReadLabels()
    {
        int offset = Header.LabelOffset;
        for (int i = 0; i < Header.NumLabels; i++)
        {
            LabelOffsets.Add(ReadInt32(offset));
            offset += 4;
        }
    }

    private void ReadFields()
    {
        int offset = Header.FieldSpecOffset;
        int specSize = Header.FieldSpecSize;

        for (int i = 0; i < Header.NumFlds; i++)
        {
            if (offset + specSize > _data.Length)
            {
                // Pad remaining fields (e.g. truncated overlay with temp fields)
                for (int j = i; j < Header.NumFlds; j++)
                    Fields.Add(new RunFieldSpec { Name = $"TEMP{j - i}", FieldType = 'A' });
                break;
            }

            var field = new RunFieldSpec
            {
                Name = ReadFieldName(offset),
                Offset = ReadInt32(offset + 15),
                FieldType = (char)_data[offset + 19],
                Decimals = _data[offset + 20],
                DisplaySize = ReadInt32(offset + 21),
                ArrayCount = ReadInt32(offset + 25),
                IsFileField = _data[offset + 29] != 0,
                PictureType = (char)_data[offset + 30],
                PictureLocation = ReadInt32(offset + 31),
                IsReset = _data[offset + 35] != 0,
                ForceUpperCase = _data[offset + 36] != 0,
                AllocFlag = _data[offset + 37],
                InternalSize = ReadInt32(offset + 38),
                KeyNumber = _data[offset + 42],
                FileBufferNumber = _data[offset + 43],
                IsReady = _data[offset + 44] != 0,
                HasInitialValue = _data[offset + 45] != 0,
                FileHandle = ReadUInt16(offset + 46),
            };

            Fields.Add(field);
            offset += specSize;
        }
    }

    /// <summary>
    /// Read a field name from a 15-byte Name field.
    /// TAS stores names either as:
    ///   - Pascal ShortString: byte[0] = length, bytes[1..length] = name (for temp fields)
    ///   - Fixed char array: 15 bytes of ASCII, space-padded (for regular fields)
    /// Heuristic: if byte[0] &lt; 0x20, treat as ShortString; otherwise fixed array.
    /// </summary>
    private string ReadFieldName(int offset)
    {
        byte firstByte = _data[offset];

        if (firstByte > 0 && firstByte < 0x20)
        {
            // ShortString format: length prefix
            int len = Math.Min((int)firstByte, 14);
            return Encoding.ASCII.GetString(_data, offset + 1, len).TrimEnd();
        }

        // Fixed char array: read 15 bytes, trim trailing spaces and nulls
        return ReadFixedString(offset, 15);
    }

    private string ReadFixedString(int offset, int length)
    {
        if (offset + length > _data.Length)
            return "";
        return Encoding.ASCII.GetString(_data, offset, length).TrimEnd('\0', ' ');
    }

    private int ReadInt32(int offset)
    {
        if (offset + 4 > _data.Length) return 0;
        return BitConverter.ToInt32(_data, offset);
    }

    private ushort ReadUInt16(int offset)
    {
        if (offset + 2 > _data.Length) return 0;
        return BitConverter.ToUInt16(_data, offset);
    }

    private byte[] ReadSegment(int offset, int size)
    {
        if (size <= 0 || offset >= _data.Length)
            return [];
        // Clamp to available data (handles truncated files like ADV50.OVL)
        int available = Math.Min(size, _data.Length - offset);
        byte[] seg = new byte[available];
        Array.Copy(_data, offset, seg, 0, available);
        return seg;
    }
}
