using System.Text;

namespace CoTAS.Parser.RunFile;

/// <summary>
/// Decodes spec segment parameters for bytecode instructions.
/// Each parameter is a 5-byte TSpecLinePtr: 1 byte type + 4 byte location.
/// Types: 'C'=constant, 'F'=field, 'X'=expression, 'N'=numeric literal,
///        'Y'=array field, 'M'=macro, 's'=single char, 'q'=macro array.
/// </summary>
public sealed class SpecDecoder
{
    private readonly RunFileReader _run;
    private ExpressionDecoder? _exprDecoder;

    public SpecDecoder(RunFileReader run)
    {
        _run = run;
    }

    /// <summary>
    /// Lazy-init expression decoder (needs back-reference to this SpecDecoder).
    /// </summary>
    private ExpressionDecoder ExprDecoder =>
        _exprDecoder ??= new ExpressionDecoder(_run, this);

    /// <summary>
    /// Read spec bytes for an instruction from the spec segment.
    /// </summary>
    public byte[] GetSpecBytes(RunBytecodeInstruction instr)
    {
        int ptr = instr.SpecLinePtr;
        int size = instr.SpecLineSize;
        if (ptr < 0 || size <= 0 || ptr + size > _run.SpecSegment.Length)
            return [];
        byte[] result = new byte[size];
        Array.Copy(_run.SpecSegment, ptr, result, 0, size);
        return result;
    }

    /// <summary>
    /// Read a 5-byte spec parameter (type + location) at given offset within spec bytes.
    /// </summary>
    public (char Type, int Location) ReadSpecParam(byte[] spec, int offset)
    {
        if (offset + 5 > spec.Length)
            return ('\0', 0);
        char typ = (char)spec[offset];
        int loc = BitConverter.ToInt32(spec, offset + 1);
        return (typ, loc);
    }

    /// <summary>
    /// Read a 4-byte int32 at given offset within spec bytes.
    /// </summary>
    public int ReadInt32(byte[] spec, int offset)
    {
        if (offset + 4 > spec.Length)
            return 0;
        return BitConverter.ToInt32(spec, offset);
    }

    /// <summary>
    /// Read a single byte at given offset within spec bytes.
    /// </summary>
    public byte ReadByte(byte[] spec, int offset)
    {
        if (offset >= spec.Length)
            return 0;
        return spec[offset];
    }

    /// <summary>
    /// Resolve a spec parameter to a human-readable string.
    /// </summary>
    public string ResolveParam(char type, int location)
    {
        switch (type)
        {
            case 'F': // Field reference — location is byte offset into field list
            {
                int fldSize = _run.Header.FieldSpecSize;
                int idx = fldSize > 0 ? location / fldSize : location;
                if (idx >= 0 && idx < _run.Fields.Count)
                {
                    string name = _run.Fields[idx].Name;
                    // If name contains non-printable chars, use FIELD[N] syntax
                    if (string.IsNullOrEmpty(name) || name.Any(c => c < ' ' || c > '~'))
                        return $"FIELD[{idx}]";
                    return name;
                }
                return $"FIELD[{idx}]";
            }

            case 'C': // Constant — location is byte offset into constant segment
                return ReadConstant(location);

            case 'N': // Numeric literal
                return location.ToString();

            case 'X': // Expression — offset into constants for compiled expression
            case 'x': // Lowercase variant (used in CASE values and some contexts)
                return ExprDecoder.Decode(location);

            case 'Y': // Array field reference
                return ResolveArrayField(location);

            case 'M': // Macro (indirect field reference - field value contains target field name)
            {
                int fldSize = _run.Header.FieldSpecSize;
                int idx = fldSize > 0 ? location / fldSize : location;
                if (idx >= 0 && idx < _run.Fields.Count)
                {
                    string name = _run.Fields[idx].Name;
                    if (!string.IsNullOrEmpty(name) && name.All(c => c >= ' ' && c <= '~'))
                        return $"@{name}";
                    return $"@FIELD[{idx}]";
                }
                return $"@FIELD[{idx}]";
            }

            case 's': // Single character constant
            {
                char ch = (char)(location & 0xFF);
                return $"'{SanitizeChar(ch)}'";
            }

            case 'q': // Macro array (indirect array field reference)
            {
                int fldSize = _run.Header.FieldSpecSize;
                int idx = fldSize > 0 ? location / fldSize : location;
                if (idx >= 0 && idx < _run.Fields.Count)
                {
                    string name = _run.Fields[idx].Name;
                    if (!string.IsNullOrEmpty(name) && name.All(c => c >= ' ' && c <= '~'))
                        return $"@{name}[]";
                    return $"@FIELD[{idx}][]";
                }
                return $"@FIELD[{idx}][]";
            }

            case 'G': // GOTO label reference — location is label index
                return $"LABEL_{location}";

            case 'S': // GOSUB label reference — location is label index
                return $"LABEL_{location}";

            case 'I': // IGNR (ignore) action
                return "IGNR";

            case 'D': // DFLT (default) action / block-IF DO marker
                return location == 0 ? "DFLT" : $"LABEL_{location}";

            case 'T': // IF-THEN marker
                return "THEN";

            case 'R': // IF-REENT marker
                return "REENT";

            case 'E': // IF-RET marker
                return "RET";

            case '\0':
            case ' ':
                return "";

            default:
                return $"?0x{(int)type:X2}:{location}";
        }
    }

    /// <summary>
    /// Resolve a 5-byte spec parameter to readable text.
    /// </summary>
    public string ResolveSpecParam(byte[] spec, int offset)
    {
        var (type, location) = ReadSpecParam(spec, offset);
        if (type == '\0' || type == ' ')
            return "";
        return ResolveParam(type, location);
    }

    /// <summary>
    /// Read a constant value from the constant segment.
    /// Format: type(1) + decimals(1) + displaySize(2) + data(...)
    /// </summary>
    private string ReadConstant(int offset)
    {
        var cs = _run.ConstantSegment;
        if (offset < 0 || offset >= cs.Length)
            return $"CONST@{offset}";

        byte cTypeByte = cs[offset];
        char cType = (char)cTypeByte;

        // If first byte is 0xFD, this is a compiled expression, not a constant
        if (cTypeByte == 0xFD)
            return ExprDecoder.Decode(offset);

        // If first byte is not a recognized constant type letter, it's likely misaligned
        if (cTypeByte < 0x20 || cTypeByte >= 0x7F)
            return $"CONST@{offset}";

        // Handle compact types that don't have the standard 4-byte header
        // L: type(1) + value(1) = 2 bytes
        // I: type(1) + uint16(2) = 3 bytes
        if (cType == 'L' && offset + 2 <= cs.Length)
            return cs[offset + 1] != 0 ? ".T." : ".F.";
        if (cType == 'I' && offset + 3 <= cs.Length)
            return BitConverter.ToUInt16(cs, offset + 1).ToString();

        // byte decimals = cs[offset + 1]; // not used in display
        int displaySize = offset + 4 <= cs.Length
            ? BitConverter.ToUInt16(cs, offset + 2)
            : 0;

        int dataStart = offset + 4;
        int dataLen = Math.Min(displaySize, cs.Length - dataStart);
        if (dataLen <= 0)
        {
            // Zero-length data: for numeric/integer types, this means the value is 0
            if (cType is 'I' or 'N' or 'R' or 'B' or 'P')
                return "0";
            if (cType is 'A' or 'a')
                return "''";
            if (cType == 'L')
                return ".F.";
            if (cType == 'D')
                return "DATE()";
            if (cType == 'T')
                return "TIME()";
            return "''";
        }
        if (dataLen > 4000)
            dataLen = 4000; // Clamp to reasonable size but still show value

        switch (cType)
        {
            case 'A': // Alpha/string
            case 'a':
            {
                // Check if this "string" is actually embedded spec params (field/const pointers).
                // TAS stores field arrays as packed TSpecLinePtr entries: type(1)+offset(4) each.
                // Detect by looking for known type markers (F,C,N,X,Y,M) with null-heavy 4-byte offsets.
                if (dataLen >= 5 && LooksLikeEmbeddedParams(cs, dataStart, dataLen))
                    return DecodeEmbeddedParams(cs, dataStart, dataLen);
                // Preserve trailing spaces for round-trip fidelity (trimEnd: false)
                string str = SanitizeBytes(cs, dataStart, dataLen, trimEnd: false);
                return $"'{str}'";
            }

            case 'I': // Integer - compact format: 'I'(1) + int16(2) = 3 bytes, no header
                if (offset + 3 <= cs.Length)
                    return BitConverter.ToInt16(cs, offset + 1).ToString();
                return "0";
            case 'B': // Byte - compact format: 'B'(1) + byte(1) = 2 bytes, no header
                if (offset + 2 <= cs.Length)
                    return cs[offset + 1].ToString();
                return "0";
            case 'R': // Real - compact format: 'R'(1) + int32(4) = 5 bytes, no header
                if (offset + 5 <= cs.Length)
                    return BitConverter.ToInt32(cs, offset + 1).ToString();
                return "0";
            case 'L': // Logical - compact format: 'L'(1) + byte(1) = 2 bytes, no header
                if (offset + 2 <= cs.Length)
                    return cs[offset + 1] != 0 ? ".T." : ".F.";
                return ".F.";
            case 'N': // Numeric - header format: 'N'(1) + dec(1) + displaySize(2) + double(8) = 12 bytes
            case 'P': // Packed
            {
                // N type has standard header, then binary double (8 bytes)
                if (offset + 12 <= cs.Length)
                    return BitConverter.ToDouble(cs, dataStart).ToString("G");
                // Fallback: try reading as ASCII if binary parse fails
                if (dataLen > 0)
                {
                    bool isAsciiNum = true;
                    for (int i = dataStart; i < dataStart + Math.Min(dataLen, 40); i++)
                    {
                        byte b = cs[i];
                        if (b == 0) break;
                        if (!((b >= '0' && b <= '9') || b == '-' || b == '+' || b == '.' || b == ' ' || b == 'E' || b == 'e'))
                        {
                            isAsciiNum = false;
                            break;
                        }
                    }
                    if (isAsciiNum)
                    {
                        string numStr = SanitizeBytes(cs, dataStart, Math.Min(dataLen, 40), trimEnd: true);
                        return numStr.Length > 0 ? numStr : "0";
                    }
                    if (dataLen >= 8) return BitConverter.ToDouble(cs, dataStart).ToString("G");
                    if (dataLen >= 4) return BitConverter.ToInt32(cs, dataStart).ToString();
                }
                return "0";
            }

            case 'D': // Date
                return $"DATE({SanitizeBytes(cs, dataStart, Math.Min(dataLen, 8), trimEnd: true)})";

            case 'T': // Time
                return $"TIME({SanitizeBytes(cs, dataStart, Math.Min(dataLen, 6), trimEnd: true)})";

            default:
            {
                // Try embedded params first
                if (dataLen >= 5 && LooksLikeEmbeddedParams(cs, dataStart, dataLen))
                    return DecodeEmbeddedParams(cs, dataStart, dataLen);
                string raw = SanitizeBytes(cs, dataStart, Math.Min(dataLen, 40), trimEnd: true);
                return raw.Length > 0 ? $"'{raw}'" : "''";
            }
        }
    }

    /// <summary>
    /// Map control bytes to readable names.
    /// Low bytes 0x01-0x0A map to TAS internal trap key codes (F1-F10).
    /// Bytes 0x0B+ map to other TAS trap key codes.
    /// </summary>
    private static readonly Dictionary<int, string> _controlCharNames = new()
    {
        [0x00] = "\\0",
        // TAS internal trap key codes (used in TRAP key string constants)
        [0x01] = "f1", [0x02] = "f2", [0x03] = "f3", [0x04] = "f4", [0x05] = "f5",
        [0x06] = "f6", [0x07] = "f7", [0x08] = "f8", [0x09] = "f9", [0x0A] = "f10",
        [0x0B] = "ESC", [0x0C] = "INT", [0x0D] = "T_ESC",
        [0x0E] = "UPAR", [0x0F] = "DNAR",
        [0x11] = "RT_A",
        [0x14] = "HOME",
        [0x17] = "PG_DN", [0x18] = "INSRT", [0x19] = "DEL_KEY",
        [0x1E] = "RSRCH",
        [0x7F] = "<DEL>",
    };

    /// <summary>
    /// TAS function key scan codes (from TRAP key definitions).
    /// </summary>
    private static readonly Dictionary<int, string> _keyNames = new()
    {
        [0x3B] = "F1", [0x3C] = "F2", [0x3D] = "F3", [0x3E] = "F4", [0x3F] = "F5",
        [0x40] = "F6", [0x41] = "F7", [0x42] = "F8", [0x43] = "F9", [0x44] = "F10",
        [0x48] = "UP", [0x50] = "DN", [0x4B] = "LEFT", [0x4D] = "RIGHT",
        [0x47] = "HOME", [0x4F] = "END", [0x49] = "PGUP", [0x51] = "PGDN",
        [0x52] = "INS", [0x53] = "DEL_KEY",
    };

    private static string SanitizeChar(char ch)
    {
        if (ch >= 0x20 && ch < 0x7F) return ch.ToString();
        if (_controlCharNames.TryGetValue(ch, out var name)) return name;
        if (_keyNames.TryGetValue(ch, out var keyName)) return $"<{keyName}>";
        // CP437 high bytes (0x80-0xFF) — pass through as raw bytes (box-drawing, accented chars, etc.)
        if (ch >= 0x80) return ch.ToString();
        return $"\\x{(int)ch:X2}";
    }

    /// <summary>
    /// Convert raw bytes to a sanitized printable string, escaping all non-printable bytes.
    /// Works directly on byte[] to avoid Encoding.ASCII silently replacing high bytes with '?'.
    /// </summary>
    private static string SanitizeBytes(byte[] data, int start, int length, bool trimEnd = false)
    {
        // Find effective end (trim trailing nulls and spaces)
        int end = start + length;
        if (trimEnd)
        {
            while (end > start && (data[end - 1] == 0x00 || data[end - 1] == 0x20))
                end--;
        }

        var sb = new StringBuilder(end - start);
        for (int i = start; i < end; i++)
        {
            byte b = data[i];
            if (b >= 0x20 && b < 0x7F)
                sb.Append((char)b);
            else
                sb.Append(SanitizeChar((char)b));
        }
        return sb.ToString();
    }

    /// <summary>
    /// Check if byte data looks like packed TSpecLinePtr entries rather than a text string.
    /// TAS stores field/value lists as sequences of type(1)+offset(4) entries.
    /// </summary>
    private bool LooksLikeEmbeddedParams(byte[] data, int start, int length)
    {
        if (length < 5) return false; // Need at least 1 5-byte param

        // Scan for strictly consecutive TSpecLinePtr entries at the start.
        // Each entry: type(1) + int32 location(4). Validate locations are plausible.
        int paramCount = 0;
        int pos = start;
        int end = start + length;
        while (pos + 5 <= end)
        {
            char type = (char)data[pos];
            if (!_validConstParamTypes.Contains(type))
                break;

            int loc = BitConverter.ToInt32(data, pos + 1);
            if (!IsPlausibleParamLocation(type, loc))
                break;

            paramCount++;
            pos += 5;
        }
        if (paramCount >= 2) return true;
        if (paramCount == 1)
        {
            // Single param: only accept if remaining bytes are all zeros/padding
            bool restIsZero = true;
            for (int i = pos; i < end; i++)
            {
                if (data[i] != 0) { restIsZero = false; break; }
            }
            return restIsZero;
        }
        return false;
    }

    /// <summary>
    /// Check if a param location value is plausible for its type.
    /// Rejects obviously wrong values (e.g., field indices out of range, offsets beyond segment).
    /// </summary>
    private bool IsPlausibleParamLocation(char type, int loc)
    {
        switch (type)
        {
            case 'F': // Field offset — must be aligned and in range
            {
                int fldSize = _run.Header.FieldSpecSize;
                if (fldSize <= 0) return loc >= 0 && loc < _run.Fields.Count;
                if (loc < 0 || loc % fldSize != 0) return false;
                int idx = loc / fldSize;
                return idx >= 0 && idx < _run.Fields.Count;
            }
            case 'C': // Constant offset — must be within constant segment
            case 'X': // Expression offset — also in constant segment
            case 'x': // Lowercase expression variant
                return loc >= 0 && loc < _run.ConstantSegment.Length;
            case 'N': // Numeric literal — any int32 is valid but very large values are suspicious
                return true; // N is always plausible
            case 'Y': // Array field — offset into constant segment (base+index, 10 bytes)
                return loc >= 0 && loc + 10 <= _run.ConstantSegment.Length;
            case 'M': // Macro — constant offset
            case 'q': // Macro array — constant offset
                return loc >= 0 && loc < _run.ConstantSegment.Length;
            case 's': // Single char — low byte is the char, should be reasonable
                return (loc & 0xFF) < 0x80; // printable or control char range
            default:
                return false;
        }
    }

    /// <summary>
    /// Decode a constant that contains embedded TSpecLinePtr params (field arrays for MENU, UPDTA, LISTF, etc.).
    /// Reads strictly consecutive 5-byte params. Stops at first byte that isn't a recognized type.
    /// For 'F' params, validates the field index resolves to a real field name.
    /// </summary>
    private string DecodeEmbeddedParams(byte[] data, int start, int length)
    {
        var parts = new List<string>();
        int end = start + length;
        int pos = start;
        while (pos + 5 <= end)
        {
            char type = (char)data[pos];
            if (!_validConstParamTypes.Contains(type))
                break;

            int loc = BitConverter.ToInt32(data, pos + 1);
            if (!IsPlausibleParamLocation(type, loc))
                break;

            string resolved = ResolveParam(type, loc);
            if (!string.IsNullOrEmpty(resolved))
                parts.Add(resolved);
            else
                break;
            pos += 5;
        }
        if (parts.Count >= 1)
            return string.Join(", ", parts);
        return "''";
    }

    private static readonly HashSet<char> _validConstParamTypes = ['F', 'C', 'N', 'X', 'x', 'Y', 'M', 's', 'q'];

    /// <summary>
    /// TAS internal trap key code to name mapping.
    /// Each trap key is stored as a single byte in the key constant string.
    /// </summary>
    private static readonly Dictionary<byte, string> _trapKeyNames = new()
    {
        [0x01] = "f1", [0x02] = "f2", [0x03] = "f3", [0x04] = "f4", [0x05] = "f5",
        [0x06] = "f6", [0x07] = "f7", [0x08] = "f8", [0x09] = "f9", [0x0A] = "f10",
        [0x0B] = "ESC", [0x0C] = "INT", [0x0D] = "T_ESC",
        [0x0E] = "UPAR", [0x0F] = "DNAR", [0x10] = "LT_A", [0x11] = "RT_A",
        [0x12] = "LT_A_AS", [0x13] = "RT_A_AS", [0x14] = "HOME", [0x15] = "END",
        [0x16] = "PG_UP", [0x17] = "PG_DN", [0x18] = "INSRT", [0x19] = "DEL_KEY",
        [0x1A] = "WD_LT", [0x1B] = "WD_RT", [0x1C] = "TAB", [0x1D] = "BCK_TAB",
        [0x1E] = "RSRCH", [0x1F] = "L_EXIT", [0x20] = "RLCK", [0x21] = "FERR",
        [0x22] = "PERR", [0x23] = "PG_BRK",
        // Shift+F keys: SF1-SF10
        [0x24] = "sf1", [0x25] = "sf2", [0x26] = "sf3", [0x27] = "sf4", [0x28] = "sf5",
        [0x29] = "sf6", [0x2A] = "sf7", [0x2B] = "sf8", [0x2C] = "sf9", [0x2D] = "sf10",
        // Ctrl+F keys: CTL_F1-CTL_F10
        [0x2E] = "ctl_f1", [0x2F] = "ctl_f2", [0x30] = "ctl_f3", [0x31] = "ctl_f4",
        [0x32] = "ctl_f5", [0x33] = "ctl_f6", [0x34] = "ctl_f7", [0x35] = "ctl_f8",
        [0x36] = "ctl_f9", [0x37] = "ctl_f10",
        [0x38] = "CTL_PG_UP", [0x39] = "CTL_PG_DN",
        // Alt+F keys: ALT_F1-ALT_F10
        [0x3A] = "alt_f1", [0x3B] = "alt_f2", [0x3C] = "alt_f3",
        // Mouse events
        [0x56] = "MOUSE_MOV", [0x57] = "MOUSE_LBD",
    };

    /// <summary>Get a trap key name from its byte code.</summary>
    public static string GetTrapKeyName(byte code) =>
        _trapKeyNames.TryGetValue(code, out var name) ? name : $"KEY_0x{code:X2}";

    /// <summary>
    /// Decode a trap key constant: the constant segment contains individual
    /// key code bytes that map to key names via _trapKeyNames.
    /// Returns comma-separated key names like "f2,f3,f4,f5".
    /// </summary>
    public string DecodeTrapKeys(char type, int location)
    {
        if (type != 'C') return ResolveParam(type, location);

        var cs = _run.ConstantSegment;
        if (location < 0 || location >= cs.Length) return $"CONST@{location}";

        // Trap key constants: type='A', then data bytes are key codes
        byte cType = cs[location];
        int dataStart, dataLen;
        if (cType == 'A' || cType == 'a')
        {
            int displaySize = location + 4 <= cs.Length
                ? BitConverter.ToUInt16(cs, location + 2) : 0;
            dataStart = location + 4;
            dataLen = Math.Min(displaySize, cs.Length - dataStart);
        }
        else
        {
            // Not a string constant, fall back
            return ResolveParam(type, location);
        }

        if (dataLen <= 0) return "";

        var keys = new List<string>();
        for (int i = 0; i < dataLen; i++)
        {
            byte b = cs[dataStart + i];
            if (b == 0) break;
            keys.Add(GetTrapKeyName(b));
        }
        return string.Join(",", keys);
    }

    /// <summary>Count escape sequences (\\x, \\0, \\t, etc.) in a sanitized string.</summary>
    private static int CountEscapes(string s)
    {
        int count = 0;
        for (int i = 0; i < s.Length - 1; i++)
        {
            if (s[i] == '\\') { count++; i++; } // skip the char after backslash
            else if (s[i] == '<' && i + 2 < s.Length) // <ESC>, <F1>, etc.
            {
                int close = s.IndexOf('>', i + 1);
                if (close > i && close - i < 8) { count++; i = close; }
            }
        }
        return count;
    }

    /// <summary>
    /// Resolve an array field reference from the constant segment.
    /// </summary>
    private string ResolveArrayField(int constOffset)
    {
        var cs = _run.ConstantSegment;
        if (constOffset < 0 || constOffset + 10 > cs.Length)
            return $"ARRAY@{constOffset}";

        // Array reference in constants: base field spec param (5 bytes) + index spec param (5 bytes)
        char baseType = (char)cs[constOffset];
        int baseLoc = BitConverter.ToInt32(cs, constOffset + 1);
        char idxType = (char)cs[constOffset + 5];
        int idxLoc = BitConverter.ToInt32(cs, constOffset + 6);

        string baseName = ResolveParam(baseType, baseLoc);
        string idxName = ResolveParam(idxType, idxLoc);
        return $"{baseName}[{idxName}]";
    }
}
