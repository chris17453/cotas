namespace CoTAS.Compiler;

/// <summary>
/// Builds the spec segment for a .RUN file.
/// Each instruction's parameters are encoded as a sequence of bytes in the spec segment.
/// The instruction stores a pointer (offset) and size into this segment.
///
/// The fundamental unit is the 5-byte TSpecLinePtr:
///   type(1) + location(4)
///
/// Types:
///   'F' = field reference (location = fieldIndex * fieldSpecSize)
///   'C' = constant (location = byte offset in constant segment)
///   'N' = numeric literal (location = the int value directly)
///   'X' = expression (location = byte offset in constant segment to compiled RPN)
///   'Y' = array field (location = byte offset in constant segment to 10-byte base+index)
///   'M' = macro / indirect field reference
///   's' = single character (location low byte = the char)
///   'q' = macro array
/// </summary>
public sealed class SpecBuilder
{
    private readonly MemoryStream _stream = new();

    /// <summary>Current size of the spec segment in bytes.</summary>
    public int Size => (int)_stream.Length;

    /// <summary>Get the built spec segment.</summary>
    public byte[] ToArray() => _stream.ToArray();

    /// <summary>
    /// Begin a new spec entry. Returns the offset where it starts.
    /// Call the Write* methods to add parameters, then call EndSpec to get the size.
    /// </summary>
    public int BeginSpec() => (int)_stream.Length;

    /// <summary>
    /// Get the size of the current spec entry (from beginOffset to current position).
    /// </summary>
    public int GetSpecSize(int beginOffset) => (int)_stream.Length - beginOffset;

    /// <summary>
    /// Write a 5-byte field reference param.
    /// </summary>
    public void WriteFieldParam(int fieldSpecOffset)
    {
        _stream.WriteByte((byte)'F');
        WriteInt32(fieldSpecOffset);
    }

    /// <summary>
    /// Write a 5-byte constant reference param.
    /// </summary>
    public void WriteConstParam(int constOffset)
    {
        _stream.WriteByte((byte)'C');
        WriteInt32(constOffset);
    }

    /// <summary>
    /// Write a 5-byte numeric literal param.
    /// </summary>
    public void WriteNumericParam(int value)
    {
        _stream.WriteByte((byte)'N');
        WriteInt32(value);
    }

    /// <summary>
    /// Write a 5-byte expression reference param.
    /// </summary>
    public void WriteExprParam(int constOffset)
    {
        _stream.WriteByte((byte)'X');
        WriteInt32(constOffset);
    }

    /// <summary>
    /// Write a 5-byte array field reference param.
    /// </summary>
    public void WriteArrayParam(int constOffset)
    {
        _stream.WriteByte((byte)'Y');
        WriteInt32(constOffset);
    }

    /// <summary>
    /// Write a 5-byte macro param.
    /// </summary>
    public void WriteMacroParam(int constOffset)
    {
        _stream.WriteByte((byte)'M');
        WriteInt32(constOffset);
    }

    /// <summary>
    /// Write a 5-byte single-char param.
    /// </summary>
    public void WriteCharParam(char ch)
    {
        _stream.WriteByte((byte)'s');
        WriteInt32(ch);
    }

    /// <summary>
    /// Write a 5-byte macro array param.
    /// </summary>
    public void WriteMacroArrayParam(int constOffset)
    {
        _stream.WriteByte((byte)'q');
        WriteInt32(constOffset);
    }

    /// <summary>
    /// Write a generic 5-byte spec param with explicit type and location.
    /// </summary>
    public void WriteParam(char type, int location)
    {
        _stream.WriteByte((byte)type);
        WriteInt32(location);
    }

    /// <summary>
    /// Write a 4-byte int32 (for jump targets, label numbers, etc.).
    /// </summary>
    public void WriteInt32(int value)
    {
        Span<byte> buf = stackalloc byte[4];
        BitConverter.TryWriteBytes(buf, value);
        _stream.Write(buf);
    }

    /// <summary>
    /// Write a single byte (for flags, variant types, etc.).
    /// </summary>
    public void WriteByte(byte value)
    {
        _stream.WriteByte(value);
    }

    /// <summary>
    /// Write raw bytes to the spec segment.
    /// </summary>
    public void WriteRaw(ReadOnlySpan<byte> data)
    {
        _stream.Write(data);
    }

    /// <summary>
    /// Write padding zeros.
    /// </summary>
    public void WritePadding(int count)
    {
        for (int i = 0; i < count; i++)
            _stream.WriteByte(0);
    }

    /// <summary>
    /// Write a 5-byte empty/null param (all zeros).
    /// </summary>
    public void WriteNullParam()
    {
        _stream.WriteByte(0);
        WriteInt32(0);
    }
}
