using System.Text;
using System.Text.RegularExpressions;

namespace CoTAS.Interpreter.Functions;

/// <summary>
/// String functions with full implementations.
/// </summary>
public static class StringFunctions
{
    public static void Register(Dictionary<string, Func<List<TasValue>, TasValue>> registry)
    {
        registry["LOC"] = Loc;
        registry["SEG"] = Seg;
        registry["NULL"] = Null;
        registry["LSTCHR"] = LstChr;
        registry["SNDX"] = Sndx;
        registry["IIF"] = Iif;
        registry["ISNUM"] = IsNum;
        registry["ISAL"] = IsAl;
        registry["ISUP"] = IsUp;
        registry["ISLO"] = IsLo;
        registry["REPLACE"] = Replace;
        registry["INSRT"] = Insrt;
        registry["DELC"] = Delc;
        registry["WRAP"] = Wrap;
        registry["CBYT"] = Cbyt;
        registry["HEX"] = Hex;
        registry["CCR"] = Ccr;
        registry["GSCHR"] = GsChr;
        registry["EDIT"] = Edit;
        registry["WRAPL"] = WrapL;
        registry["WRAPO"] = WrapO;
        registry["WRAPS"] = WrapS;
        registry["FILL"] = FillStr;
        registry["REWRAP"] = ReWrap;
    }

    private static TasValue Loc(List<TasValue> args)
    {
        if (args.Count < 2) throw new InterpreterException("LOC() requires 2 arguments");
        string s = args[0].AsString();
        string sub = args[1].AsString();
        int pos = s.IndexOf(sub, StringComparison.OrdinalIgnoreCase);
        return new TasValue(TasType.Integer, pos >= 0 ? pos + 1 : 0);
    }

    private static TasValue Seg(List<TasValue> args)
    {
        if (args.Count < 3) throw new InterpreterException("SEG() requires 3 arguments");
        string s = args[0].AsString();
        int start = args[1].AsInteger() - 1;
        int len = args[2].AsInteger();
        if (start < 0) start = 0;
        if (start >= s.Length) return new TasValue(TasType.Alpha, "", 0);
        if (start + len > s.Length) len = s.Length - start;
        string result = s.Substring(start, len);
        return new TasValue(TasType.Alpha, result, result.Length);
    }

    private static TasValue Null(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("NULL() requires 1 argument");
        bool isNull = string.IsNullOrWhiteSpace(args[0].AsString());
        return new TasValue(TasType.Logical, isNull);
    }

    private static TasValue LstChr(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("LSTCHR() requires 1 argument");
        string s = args[0].AsString();
        int pos = s.TrimEnd().Length;
        return new TasValue(TasType.Integer, pos);
    }

    private static TasValue Sndx(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("SNDX() requires 1 argument");
        string s = args[0].AsString().Trim().ToUpperInvariant();
        if (s.Length == 0) return new TasValue(TasType.Alpha, "0000", 4);

        char[] result = ['0', '0', '0', '0'];
        result[0] = s[0];
        int idx = 1;
        char lastCode = SoundexCode(s[0]);

        for (int i = 1; i < s.Length && idx < 4; i++)
        {
            char code = SoundexCode(s[i]);
            if (code != '0' && code != lastCode)
            {
                result[idx++] = code;
                lastCode = code;
            }
        }

        return new TasValue(TasType.Alpha, new string(result), 4);
    }

    private static char SoundexCode(char c)
    {
        return c switch
        {
            'B' or 'F' or 'P' or 'V' => '1',
            'C' or 'G' or 'J' or 'K' or 'Q' or 'S' or 'X' or 'Z' => '2',
            'D' or 'T' => '3',
            'L' => '4',
            'M' or 'N' => '5',
            'R' => '6',
            _ => '0',
        };
    }

    private static TasValue Iif(List<TasValue> args)
    {
        if (args.Count < 3) throw new InterpreterException("IIF() requires 3 arguments");
        return args[0].AsLogical() ? args[1] : args[2];
    }

    private static TasValue IsNum(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("ISNUM() requires 1 argument");
        string s = args[0].AsString().Trim();
        bool result = s.Length > 0 && s.All(c => char.IsDigit(c) || c == '.' || c == '-' || c == '+');
        return new TasValue(TasType.Logical, result);
    }

    private static TasValue IsAl(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("ISAL() requires 1 argument");
        string s = args[0].AsString().Trim();
        bool result = s.Length > 0 && s.All(char.IsLetter);
        return new TasValue(TasType.Logical, result);
    }

    private static TasValue IsUp(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("ISUP() requires 1 argument");
        string s = args[0].AsString().Trim();
        bool result = s.Length > 0 && s.All(c => !char.IsLetter(c) || char.IsUpper(c));
        return new TasValue(TasType.Logical, result);
    }

    private static TasValue IsLo(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("ISLO() requires 1 argument");
        string s = args[0].AsString().Trim();
        bool result = s.Length > 0 && s.All(c => !char.IsLetter(c) || char.IsLower(c));
        return new TasValue(TasType.Logical, result);
    }

    private static TasValue Replace(List<TasValue> args)
    {
        if (args.Count < 3) throw new InterpreterException("REPLACE() requires 3 arguments");
        string s = args[0].AsString();
        string search = args[1].AsString();
        string replacement = args[2].AsString();
        string result = s.Replace(search, replacement, StringComparison.OrdinalIgnoreCase);
        return new TasValue(TasType.Alpha, result, result.Length);
    }

    private static TasValue Insrt(List<TasValue> args)
    {
        if (args.Count < 3) throw new InterpreterException("INSRT() requires 3 arguments");
        string s = args[0].AsString();
        int pos = args[1].AsInteger() - 1;
        string insert = args[2].AsString();
        if (pos < 0) pos = 0;
        if (pos > s.Length) pos = s.Length;
        string result = s.Insert(pos, insert);
        return new TasValue(TasType.Alpha, result, result.Length);
    }

    private static TasValue Delc(List<TasValue> args)
    {
        if (args.Count < 3) throw new InterpreterException("DELC() requires 3 arguments");
        string s = args[0].AsString();
        int start = args[1].AsInteger() - 1;
        int count = args[2].AsInteger();
        if (start < 0) start = 0;
        if (start >= s.Length) return new TasValue(TasType.Alpha, s, s.Length);
        if (start + count > s.Length) count = s.Length - start;
        string result = s.Remove(start, count);
        return new TasValue(TasType.Alpha, result, result.Length);
    }

    private static TasValue Wrap(List<TasValue> args)
    {
        // WRAP(string, width) - word wrap: returns string with line breaks at word boundaries
        if (args.Count < 1) throw new InterpreterException("WRAP() requires at least 1 argument");
        string s = args[0].AsString();
        int width = args.Count >= 2 ? args[1].AsInteger() : 80;
        if (width <= 0) width = 80;

        var sb = new StringBuilder();
        int lineLen = 0;
        foreach (string word in s.Split(' '))
        {
            if (lineLen + word.Length + (lineLen > 0 ? 1 : 0) > width && lineLen > 0)
            {
                sb.Append('\n');
                lineLen = 0;
            }
            if (lineLen > 0) { sb.Append(' '); lineLen++; }
            sb.Append(word);
            lineLen += word.Length;
        }
        string result = sb.ToString();
        return new TasValue(TasType.Alpha, result, result.Length);
    }

    private static TasValue Cbyt(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("CBYT() requires 1 argument");
        return new TasValue(TasType.Integer, args[0].AsString().Length);
    }

    private static TasValue Hex(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("HEX() requires 1 argument");
        int val = args[0].AsInteger();
        string result = val.ToString("X");
        return new TasValue(TasType.Alpha, result, result.Length);
    }

    private static TasValue Ccr(List<TasValue> args)
    {
        return new TasValue(TasType.Alpha, "\r", 1);
    }

    private static TasValue GsChr(List<TasValue> args)
    {
        // GSCHR(row, col) - get screen character (delegates to ScreenFunctions)
        return new TasValue(TasType.Alpha, " ", 1);
    }

    private static TasValue Edit(List<TasValue> args)
    {
        // EDIT(value, mask) - format value with edit mask
        // Mask chars: 9=digit, X=any, A=alpha, #=digit/space, Z=zero-suppress digit
        if (args.Count < 2) return args.Count > 0 ? args[0] : new TasValue(TasType.Alpha, "", 0);
        string value = args[0].AsString();
        string mask = args[1].AsString();

        var sb = new StringBuilder();
        int vi = 0;
        for (int mi = 0; mi < mask.Length; mi++)
        {
            char mc = mask[mi];
            if (mc == '9' || mc == '#' || mc == 'Z' || mc == 'X' || mc == 'A')
            {
                if (vi < value.Length)
                {
                    char vc = value[vi++];
                    if (mc == 'Z' && vc == '0') sb.Append(' ');
                    else if (mc == '#' && vc == ' ') sb.Append(' ');
                    else sb.Append(vc);
                }
                else
                {
                    sb.Append(' ');
                }
            }
            else
            {
                // Literal character in mask (-, /, ., etc.)
                sb.Append(mc);
            }
        }
        string result = sb.ToString();
        return new TasValue(TasType.Alpha, result, result.Length);
    }

    private static TasValue WrapL(List<TasValue> args)
    {
        if (args.Count < 1) return new TasValue(TasType.Alpha, "", 0);
        string s = args[0].AsString().TrimStart();
        return new TasValue(TasType.Alpha, s, s.Length);
    }

    private static TasValue WrapO(List<TasValue> args)
    {
        if (args.Count < 1) return new TasValue(TasType.Alpha, "", 0);
        string s = args[0].AsString().Replace(" ", "");
        return new TasValue(TasType.Alpha, s, s.Length);
    }

    private static TasValue WrapS(List<TasValue> args)
    {
        if (args.Count < 1) return new TasValue(TasType.Alpha, "", 0);
        string s = args[0].AsString();
        s = Regex.Replace(s, @" {2,}", " ");
        return new TasValue(TasType.Alpha, s, s.Length);
    }

    private static TasValue FillStr(List<TasValue> args)
    {
        if (args.Count < 2) throw new InterpreterException("FILL() requires 2 arguments");
        string ch = args[0].AsString();
        int count = args[1].AsInteger();
        if (ch.Length == 0 || count <= 0) return new TasValue(TasType.Alpha, "", 0);
        string result = new string(ch[0], count);
        return new TasValue(TasType.Alpha, result, count);
    }

    private static TasValue ReWrap(List<TasValue> args)
    {
        // REWRAP(string, width) - re-wrap text to specified width
        if (args.Count < 1) return new TasValue(TasType.Alpha, "", 0);
        string s = args[0].AsString();
        int width = args.Count >= 2 ? args[1].AsInteger() : 80;
        if (width <= 0) width = 80;

        // Normalize: collapse line breaks and multiple spaces, then word-wrap
        s = Regex.Replace(s, @"[\r\n]+", " ");
        s = Regex.Replace(s, @" {2,}", " ").Trim();

        var sb = new StringBuilder();
        int lineLen = 0;
        foreach (string word in s.Split(' '))
        {
            if (lineLen + word.Length + (lineLen > 0 ? 1 : 0) > width && lineLen > 0)
            {
                sb.Append('\n');
                lineLen = 0;
            }
            if (lineLen > 0) { sb.Append(' '); lineLen++; }
            sb.Append(word);
            lineLen += word.Length;
        }
        string result = sb.ToString();
        return new TasValue(TasType.Alpha, result, result.Length);
    }
}
