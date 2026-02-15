using System.Text;
using CoTAS.Parser.RunFile;

namespace CoTAS.Compiler;

/// <summary>
/// Assembles TAS .RUN binary files from segments.
/// Reverse of <see cref="RunFileReader"/>.
///
/// File layout:
///   [128 bytes]    Header
///   [1600 bytes]   Buffer list (100 x 16 bytes)
///   [CodeSize]     Instructions (7 bytes each for TAS32)
///   [ConstSize]    Constant segment
///   [SpecSize]     Spec segment
///   [LabelSize]    Label offsets (4 bytes each)
///   [FldNameSize]  Field specs (48 bytes each for TAS32)
/// </summary>
public sealed class RunFileWriter
{
    private readonly MemoryStream _output = new();
    private readonly BinaryWriter _writer;

    public RunFileWriter()
    {
        _writer = new BinaryWriter(_output, Encoding.ASCII, leaveOpen: true);
    }

    /// <summary>
    /// Write a complete .RUN file from a RunFileReader (round-trip: read → write → identical bytes).
    /// Uses raw segment bytes for perfect fidelity.
    /// </summary>
    public static byte[] WriteFromReader(RunFileReader run)
    {
        var writer = new RunFileWriter();

        // Use raw bytes for all segments to ensure byte-identical round-trip.
        // This preserves unknown header fields, buffer slot layout, field name encoding, etc.
        writer._writer.Write(run.RawHeader);
        writer._writer.Write(run.RawBufferList);
        writer._writer.Write(run.CodeSegment);
        writer._writer.Write(run.ConstantSegment);
        writer._writer.Write(run.SpecSegment);
        writer._writer.Write(run.LabelSegment);
        writer._writer.Write(run.FieldSpecSegment);

        writer._writer.Flush();
        return writer._output.ToArray();
    }

    /// <summary>
    /// Write a complete .RUN file from individual segments.
    /// </summary>
    public static byte[] Write(
        RunFileHeader header,
        List<RunBufferEntry> buffers,
        byte[] codeSegment,
        byte[] constantSegment,
        byte[] specSegment,
        List<int> labelOffsets,
        byte[] fieldSegment)
    {
        var writer = new RunFileWriter();

        byte[] labelSeg = BuildLabelSegment(labelOffsets);
        byte[] bufferList = BuildBufferList(buffers);

        writer.WriteHeader(header, codeSegment.Length, constantSegment.Length,
            specSegment.Length, labelSeg.Length, fieldSegment.Length);
        writer._writer.Write(bufferList);
        writer._writer.Write(codeSegment);
        writer._writer.Write(constantSegment);
        writer._writer.Write(specSegment);
        writer._writer.Write(labelSeg);
        writer._writer.Write(fieldSegment);

        writer._writer.Flush();
        return writer._output.ToArray();
    }

    private void WriteHeader(RunFileHeader h, int codeSize, int constSize,
        int specSize, int labelSize, int fldNameSize)
    {
        // Ensure we write exactly 128 bytes
        byte[] header = new byte[RunFileHeader.Size];
        using var ms = new MemoryStream(header);
        using var w = new BinaryWriter(ms);

        w.Write(codeSize);                  // offset 0
        w.Write(constSize);                 // offset 4
        w.Write(specSize);                  // offset 8
        w.Write(labelSize);                 // offset 12
        w.Write(h.ScrnFldNum);              // offset 16
        w.Write(h.NumFlds);                 // offset 20
        w.Write(h.TempFlds);               // offset 24
        w.Write(h.NumTempFlds);             // offset 28
        w.Write(fldNameSize);               // offset 32
        w.Write(h.TempFldSize);             // offset 36
        w.Write(h.DefFldSegSize);           // offset 40
        w.Write(h.NumExtraFlds);            // offset 44
        w.Write(h.PrgNames);               // offset 48
        w.Write((byte)(h.DebugFlg ? 1 : 0)); // offset 52

        // ProType: 5 bytes at offset 53
        byte[] proType = new byte[5];
        Encoding.ASCII.GetBytes(h.ProType, 0, Math.Min(h.ProType.Length, 5), proType, 0);
        w.Write(proType);                   // offset 53

        w.Write(h.NumLabels);               // offset 58
        // offset 62: NewFldSpec
        ms.Position = 62;
        w.Write((byte)(h.NewFldSpec ? 1 : 0));
        w.Write((byte)(h.ChkUpVld ? 1 : 0)); // offset 63
        w.Write((byte)(h.IncLabels ? 1 : 0)); // offset 64

        // Rest is zeros (already initialized)
        _writer.Write(header);
    }

    private static byte[] BuildCodeSegment(List<RunBytecodeInstruction> instructions, bool isTas51)
    {
        int instrSize = isTas51 ? RunFileHeader.Tas51InstructionSize : RunFileHeader.Tas60InstructionSize;
        byte[] code = new byte[instructions.Count * instrSize];
        using var ms = new MemoryStream(code);
        using var w = new BinaryWriter(ms);

        foreach (var instr in instructions)
        {
            if (isTas51)
            {
                // TAS 5.1: word CmdNum (2) + byte SLSize (1) + int32 SLPtr (4) = 7 bytes
                w.Write(instr.CommandNumber);
                w.Write(instr.SpecLineSize);
                w.Write(instr.SpecLinePtr);
            }
            else
            {
                // TAS 6.0: word CmdNum (2) + byte Exit (1) + byte SLSize (1) + int32 SLPtr (4) = 8 bytes
                w.Write(instr.CommandNumber);
                w.Write(instr.Exit);
                w.Write(instr.SpecLineSize);
                w.Write(instr.SpecLinePtr);
            }
        }

        return code;
    }

    private static byte[] BuildLabelSegment(List<int> labelOffsets)
    {
        byte[] labels = new byte[labelOffsets.Count * 4];
        for (int i = 0; i < labelOffsets.Count; i++)
            BitConverter.TryWriteBytes(labels.AsSpan(i * 4), labelOffsets[i]);
        return labels;
    }

    private static byte[] BuildFieldSegment(List<RunFieldSpec> fields, RunFileHeader header)
    {
        int specSize = header.FieldSpecSize;
        byte[] segment = new byte[fields.Count * specSize];

        for (int i = 0; i < fields.Count; i++)
        {
            int offset = i * specSize;
            WriteFieldSpec(segment, offset, fields[i], specSize);
        }

        return segment;
    }

    private static void WriteFieldSpec(byte[] buf, int offset, RunFieldSpec field, int specSize)
    {
        // Name: 15 bytes
        // Detect if this should be ShortString format (temp fields) or fixed array
        if (field.IsTempField && field.Name.Length < 15)
        {
            // Pascal ShortString: length prefix byte + name bytes
            buf[offset] = (byte)field.Name.Length;
            Encoding.ASCII.GetBytes(field.Name, 0, field.Name.Length, buf, offset + 1);
        }
        else
        {
            // Fixed char array: 15 bytes, space-padded
            byte[] nameBytes = new byte[15];
            // Fill with spaces first for padding
            for (int j = 0; j < 15; j++) nameBytes[j] = (byte)' ';
            int len = Math.Min(field.Name.Length, 15);
            Encoding.ASCII.GetBytes(field.Name, 0, len, nameBytes, 0);
            Array.Copy(nameBytes, 0, buf, offset, 15);
        }

        // Offset (4 bytes at +15)
        BitConverter.TryWriteBytes(buf.AsSpan(offset + 15), field.Offset);

        // FieldType (char at +19)
        buf[offset + 19] = (byte)field.FieldType;

        // Decimals (byte at +20)
        buf[offset + 20] = (byte)field.Decimals;

        // DisplaySize (4 bytes at +21)
        BitConverter.TryWriteBytes(buf.AsSpan(offset + 21), field.DisplaySize);

        // ArrayCount (4 bytes at +25)
        BitConverter.TryWriteBytes(buf.AsSpan(offset + 25), field.ArrayCount);

        // IsFileField (byte at +29)
        buf[offset + 29] = (byte)(field.IsFileField ? 1 : 0);

        // PictureType (char at +30)
        buf[offset + 30] = (byte)field.PictureType;

        // PictureLocation (4 bytes at +31)
        BitConverter.TryWriteBytes(buf.AsSpan(offset + 31), field.PictureLocation);

        // IsReset (byte at +35)
        buf[offset + 35] = (byte)(field.IsReset ? 1 : 0);

        // ForceUpperCase (byte at +36)
        buf[offset + 36] = (byte)(field.ForceUpperCase ? 1 : 0);

        // AllocFlag (byte at +37)
        buf[offset + 37] = field.AllocFlag;

        // InternalSize (4 bytes at +38)
        BitConverter.TryWriteBytes(buf.AsSpan(offset + 38), field.InternalSize);

        // KeyNumber (byte at +42)
        buf[offset + 42] = field.KeyNumber;

        // FileBufferNumber (byte at +43)
        buf[offset + 43] = field.FileBufferNumber;

        // IsReady (byte at +44)
        buf[offset + 44] = (byte)(field.IsReady ? 1 : 0);

        // HasInitialValue (byte at +45)
        buf[offset + 45] = (byte)(field.HasInitialValue ? 1 : 0);

        // FileHandle (ushort at +46)
        BitConverter.TryWriteBytes(buf.AsSpan(offset + 46), field.FileHandle);
    }

    private static byte[] BuildBufferList(List<RunBufferEntry> buffers)
    {
        // 100 entries x 16 bytes = 1600 bytes
        byte[] list = new byte[RunFileHeader.BufferListSize];

        // Build a full 100-slot list. The reader only adds non-empty entries,
        // so we need to place them by their original index.
        // Since we don't track original buffer slot indices, we write them sequentially
        // and pad the rest with zeros (matching original behavior for files with
        // contiguous buffers starting at slot 0).
        for (int i = 0; i < buffers.Count && i < 100; i++)
        {
            int offset = i * 16;
            byte[] nameBytes = new byte[8];
            int len = Math.Min(buffers[i].Name.Length, 8);
            Encoding.ASCII.GetBytes(buffers[i].Name, 0, len, nameBytes, 0);
            Array.Copy(nameBytes, 0, list, offset, 8);
            BitConverter.TryWriteBytes(list.AsSpan(offset + 8), buffers[i].BufferPtr);
            BitConverter.TryWriteBytes(list.AsSpan(offset + 12), buffers[i].FileHandle);
        }

        return list;
    }
}
