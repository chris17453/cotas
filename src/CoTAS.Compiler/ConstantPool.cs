using System.Text;

namespace CoTAS.Compiler;

/// <summary>
/// Builds the constant segment for a .RUN file.
/// Constants are stored as: type(1) + decimals(1) + displaySize(2) + data(displaySize).
/// Deduplicates identical constant entries and returns byte offsets.
/// </summary>
public sealed class ConstantPool
{
    private readonly MemoryStream _stream = new();
    private readonly Dictionary<string, int> _dedup = new(StringComparer.Ordinal);

    /// <summary>Current size of the constant pool in bytes.</summary>
    public int Size => (int)_stream.Length;

    /// <summary>Get the built constant segment.</summary>
    public byte[] ToArray() => _stream.ToArray();

    /// <summary>
    /// Add a string constant. Returns byte offset in constant segment.
    /// Format: 'A'(1) + dec(1) + displaySize(2) + string_data(displaySize)
    /// </summary>
    public int AddString(string value)
    {
        string key = $"A:{value}";
        if (_dedup.TryGetValue(key, out int existing))
            return existing;

        int offset = (int)_stream.Length;
        byte[] data = Encoding.ASCII.GetBytes(value);
        WriteConstantHeader('A', 0, data.Length);
        _stream.Write(data);

        _dedup[key] = offset;
        return offset;
    }

    /// <summary>
    /// Add an integer constant stored as ASCII text.
    /// Format: 'I'(1) + dec(1) + displaySize(2) + ascii_digits(displaySize)
    /// </summary>
    public int AddInteger(int value)
    {
        string text = value.ToString();
        string key = $"I:{text}";
        if (_dedup.TryGetValue(key, out int existing))
            return existing;

        int offset = (int)_stream.Length;
        byte[] data = Encoding.ASCII.GetBytes(text);
        WriteConstantHeader('I', 0, data.Length);
        _stream.Write(data);

        _dedup[key] = offset;
        return offset;
    }

    /// <summary>
    /// Add a numeric constant (with decimals) stored as ASCII text.
    /// Format: 'N'(1) + dec(1) + displaySize(2) + ascii_digits(displaySize)
    /// </summary>
    public int AddNumeric(double value, int decimals)
    {
        string text = decimals > 0
            ? value.ToString($"F{decimals}")
            : value.ToString("G");
        string key = $"N:{decimals}:{text}";
        if (_dedup.TryGetValue(key, out int existing))
            return existing;

        int offset = (int)_stream.Length;
        byte[] data = Encoding.ASCII.GetBytes(text);
        WriteConstantHeader('N', (byte)decimals, data.Length);
        _stream.Write(data);

        _dedup[key] = offset;
        return offset;
    }

    /// <summary>
    /// Add a date constant as text.
    /// Format: 'D'(1) + dec(1) + displaySize(2) + date_text(displaySize)
    /// </summary>
    public int AddDate(string dateText)
    {
        string key = $"D:{dateText}";
        if (_dedup.TryGetValue(key, out int existing))
            return existing;

        int offset = (int)_stream.Length;
        byte[] data = Encoding.ASCII.GetBytes(dateText);
        WriteConstantHeader('D', 0, data.Length);
        _stream.Write(data);

        _dedup[key] = offset;
        return offset;
    }

    /// <summary>
    /// Add a time constant as text.
    /// Format: 'T'(1) + dec(1) + displaySize(2) + time_text(displaySize)
    /// </summary>
    public int AddTime(string timeText)
    {
        string key = $"T:{timeText}";
        if (_dedup.TryGetValue(key, out int existing))
            return existing;

        int offset = (int)_stream.Length;
        byte[] data = Encoding.ASCII.GetBytes(timeText);
        WriteConstantHeader('T', 0, data.Length);
        _stream.Write(data);

        _dedup[key] = offset;
        return offset;
    }

    /// <summary>
    /// Add a logical constant.
    /// Format: 'L'(1) + dec(1) + displaySize(2) + byte(1)
    /// </summary>
    public int AddLogical(bool value)
    {
        string key = $"L:{value}";
        if (_dedup.TryGetValue(key, out int existing))
            return existing;

        int offset = (int)_stream.Length;
        WriteConstantHeader('L', 0, 1);
        _stream.WriteByte(value ? (byte)1 : (byte)0);

        _dedup[key] = offset;
        return offset;
    }

    /// <summary>
    /// Add a raw byte sequence to the constant pool.
    /// Used for compiled expressions, embedded params, etc.
    /// Returns the offset where the data was written.
    /// </summary>
    public int AddRaw(byte[] data)
    {
        int offset = (int)_stream.Length;
        _stream.Write(data);
        return offset;
    }

    /// <summary>
    /// Add a compiled expression (RPN bytecode) to the constant pool.
    /// Format: type(1) + dec(1) + displaySize(2) + 0xFD(1) + tempBase(4) + ops + 0xFF
    /// displaySize covers from 0xFD to 0xFF inclusive.
    /// </summary>
    public int AddExpression(char resultType, byte decimals, byte[] rpnBody, int tempBase)
    {
        int offset = (int)_stream.Length;
        // displaySize = FD marker(1) + tempBase(4) + body + FF terminator(1)
        int displaySize = 1 + 4 + rpnBody.Length + 1;

        WriteConstantHeader(resultType, decimals, displaySize);
        _stream.WriteByte(0xFD);
        WriteInt32(tempBase);
        _stream.Write(rpnBody);
        _stream.WriteByte(0xFF);

        return offset;
    }

    /// <summary>
    /// Add an array field reference to the constant pool.
    /// Format: base_spec(5) + index_spec(5) = 10 bytes.
    /// </summary>
    public int AddArrayRef(byte baseType, int baseLoc, byte indexType, int indexLoc)
    {
        string key = $"Y:{baseType}:{baseLoc}:{indexType}:{indexLoc}";
        if (_dedup.TryGetValue(key, out int existing))
            return existing;

        int offset = (int)_stream.Length;
        _stream.WriteByte(baseType);
        WriteInt32(baseLoc);
        _stream.WriteByte(indexType);
        WriteInt32(indexLoc);

        _dedup[key] = offset;
        return offset;
    }

    /// <summary>
    /// Add an embedded parameter list (field/value arrays for MENU, UPDTA, etc.).
    /// Each param is 5 bytes: type(1) + location(4).
    /// </summary>
    public int AddEmbeddedParams(List<(byte Type, int Location)> paramList)
    {
        int totalSize = paramList.Count * 5;
        byte[] data = new byte[totalSize];
        for (int i = 0; i < paramList.Count; i++)
        {
            data[i * 5] = paramList[i].Type;
            BitConverter.TryWriteBytes(data.AsSpan(i * 5 + 1), paramList[i].Location);
        }

        // Wrap in a constant header of type 'A'
        string key = $"EP:{Convert.ToBase64String(data)}";
        if (_dedup.TryGetValue(key, out int existing))
            return existing;

        int offset = (int)_stream.Length;
        WriteConstantHeader('A', 0, totalSize);
        _stream.Write(data);

        _dedup[key] = offset;
        return offset;
    }

    private void WriteConstantHeader(char type, byte decimals, int displaySize)
    {
        _stream.WriteByte((byte)type);
        _stream.WriteByte(decimals);
        // displaySize as ushort (2 bytes, little-endian)
        _stream.WriteByte((byte)(displaySize & 0xFF));
        _stream.WriteByte((byte)((displaySize >> 8) & 0xFF));
    }

    private void WriteInt32(int value)
    {
        Span<byte> buf = stackalloc byte[4];
        BitConverter.TryWriteBytes(buf, value);
        _stream.Write(buf);
    }
}
