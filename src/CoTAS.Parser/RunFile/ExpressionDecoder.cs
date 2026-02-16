using System.Text;

namespace CoTAS.Parser.RunFile;

/// <summary>
/// Decodes compiled TAS expressions (RPN bytecode) back to infix notation.
/// Expression format in constant segment:
///   [type 'A'(1)] [dec(1)] [displaySize(2)] [0xFD(1)] [tempBase(4)] [operations...] [0xFF]
/// Operation types:
///   0x00: binary op — 00 + operator(1) + lhs(5) + rhs(5) + result(4) = 16 bytes
///   0x01: 0-arg function — 01 + funcNum(1) + result(4) = 6 bytes
///   0x02: 1-arg function — 02 + funcNum(1) + arg(5) + result(4) = 11 bytes
///   0x03+: N-arg function — (N+1) + funcNum(1) + args(5*N) + result(4)
///   0xFF: terminator
/// Results are stored as field spec offsets (temps) and referenced by later ops via F@offset.
/// </summary>
public sealed class ExpressionDecoder
{
    private readonly RunFileReader _run;
    private readonly SpecDecoder _spec;

    public ExpressionDecoder(RunFileReader run, SpecDecoder spec)
    {
        _run = run;
        _spec = spec;
    }

    /// <summary>
    /// Decode a compiled expression at the given constant segment offset.
    /// Returns an infix string representation.
    /// </summary>
    public string Decode(int offset)
    {
        var cs = _run.ConstantSegment;
        if (offset < 0 || offset + 4 >= cs.Length)
            return $"EXPR@{offset}";

        // Skip constant header: type(1) + dec(1) + displaySize(2)
        int displaySize = BitConverter.ToUInt16(cs, offset + 2);
        int exprStart = offset + 4;

        if (exprStart >= cs.Length || cs[exprStart] != 0xFD)
            return $"EXPR@{offset}";

        // Skip FD marker + tempBase(4)
        int rpos = exprStart + 5;
        int exprEnd = exprStart + displaySize;
        if (exprEnd > cs.Length) exprEnd = cs.Length;

        // Track temp slot values as expression strings
        var temps = new Dictionary<int, string>();
        string lastResult = "";

        int safety = 0;
        while (rpos < exprEnd && cs[rpos] != 0xFF && safety++ < 100)
        {
            byte opType = cs[rpos];

            if (opType == 0x00) // Binary operation
            {
                if (rpos + 16 > exprEnd) break;
                byte op = cs[rpos + 1];
                string lhs = ResolveOperand(cs, rpos + 2, temps);
                string rhs = ResolveOperand(cs, rpos + 7, temps);
                int resultOff = BitConverter.ToInt32(cs, rpos + 12);

                string opStr = GetBinaryOperator(op);
                bool needsParens = IsComparisonOp(op);

                string result;
                if (opStr == "+" && IsStringConcat(lhs, rhs))
                    result = $"{lhs}+{rhs}";
                else if (needsParens && ContainsBinaryOp(lhs))
                    result = $"({lhs}){opStr}{rhs}";
                else
                    result = $"{lhs}{opStr}{rhs}";

                temps[resultOff] = result;
                lastResult = result;
                rpos += 16;
            }
            else if (opType >= 0x01 && opType <= 0x09) // Function call
            {
                if (rpos + 2 > exprEnd) break;
                byte funcNum = cs[rpos + 1];
                int numArgs = opType - 1;

                var args = new List<string>();
                int argPos = rpos + 2;
                for (int a = 0; a < numArgs; a++)
                {
                    if (argPos + 5 > exprEnd) break;
                    args.Add(ResolveOperand(cs, argPos, temps));
                    argPos += 5;
                }
                if (argPos + 4 > exprEnd) break;
                int resultOff = BitConverter.ToInt32(cs, argPos);

                string funcName = GetFunctionName(funcNum);
                string result = numArgs > 0
                    ? $"{funcName}({string.Join(",", args)})"
                    : $"{funcName}()";

                temps[resultOff] = result;
                lastResult = result;
                rpos = argPos + 4;
            }
            else if (opType == 0x0C || opType == 0x0D || opType == 0x0E ||
                     opType == 0x0F || opType == 0x11 || opType == 0x14 ||
                     (opType >= 0x10 && opType <= 0x1F && opType != 0x0A && opType != 0x0B))
            {
                // UDF/FUNC call: opType label(4) padding(4) args(5*N) result(4)
                // 0x0C is standard, 0x0D/0x0E/0x0F/0x11/0x14+ are variant UDF calls
                if (rpos + 9 > exprEnd) break;
                int labelIdx = BitConverter.ToInt32(cs, rpos + 1);
                // Skip 4-byte padding/metadata after label
                int argPos = rpos + 9;

                // Scan for TSpecLinePtr args (valid type byte at start)
                var args = new List<string>();
                while (argPos + 5 <= exprEnd)
                {
                    char argType = (char)cs[argPos];
                    if (!IsValidParamType(argType)) break;
                    args.Add(ResolveOperand(cs, argPos, temps));
                    argPos += 5;
                }

                // Read 4-byte result offset
                if (argPos + 4 > exprEnd) break;
                int resultOff = BitConverter.ToInt32(cs, argPos);

                string labelName = labelIdx >= 0 && labelIdx < _run.LabelOffsets.Count
                    ? $"LABEL_{labelIdx}" : $"FUNC@{labelIdx}";
                string result = args.Count > 0
                    ? $"{labelName}({string.Join(",", args)})"
                    : $"{labelName}()";

                temps[resultOff] = result;
                lastResult = result;
                rpos = argPos + 4;
            }
            else if (opType == 0xB4 || opType == 0xB5 || opType == 0xB6) // Array element access: B4/B5/B6 field(5) indexOffset(4) result(4) = 14 bytes
            {
                if (rpos + 14 > exprEnd) break;
                string arrayField = ResolveOperand(cs, rpos + 1, temps);
                int indexOff = BitConverter.ToInt32(cs, rpos + 6);
                int resultOff = BitConverter.ToInt32(cs, rpos + 10);

                // indexOff is a field spec offset for the index variable
                int fldSize = _run.Header.FieldSpecSize;
                string indexStr;
                if (fldSize > 0 && indexOff >= 0)
                {
                    int idx = indexOff / fldSize;
                    if (idx >= 0 && idx < _run.Fields.Count)
                    {
                        string fname = _run.Fields[idx].Name;
                        indexStr = (string.IsNullOrEmpty(fname) || fname.Any(c => c < ' ' || c > '~'))
                            ? $"FIELD[{idx}]" : fname;
                    }
                    else
                        indexStr = indexOff.ToString();
                }
                else
                    indexStr = indexOff.ToString();

                string result = $"{arrayField}[{indexStr}]";
                temps[resultOff] = result;
                lastResult = result;
                rpos += 14;
            }
            else if (opType == 0x0A || opType == 0x0B)
            {
                // Standalone comparison ops (<> and >=) with abbreviated temp references:
                // op(1) + lhsTemp(4) + rhsTemp(4) + resultTemp(4) = 13 bytes
                if (rpos + 13 > exprEnd) break;
                int lhsOff = BitConverter.ToInt32(cs, rpos + 1);
                int rhsOff = BitConverter.ToInt32(cs, rpos + 5);
                int resultOff = BitConverter.ToInt32(cs, rpos + 9);

                string lhs = temps.TryGetValue(lhsOff, out var l) ? l : ResolveFieldOffset(lhsOff);
                string rhs = temps.TryGetValue(rhsOff, out var r) ? r : ResolveFieldOffset(rhsOff);
                string opStr = GetBinaryOperator(opType);

                string result = $"{lhs}{opStr}{rhs}";
                temps[resultOff] = result;
                lastResult = result;
                rpos += 13;
            }
            else
            {
                // Unknown operation type — bail
                break;
            }
        }

        return lastResult.Length > 0 ? lastResult : $"EXPR@{offset}";
    }

    /// <summary>
    /// Try to resolve a raw offset as a field spec offset.
    /// Standalone comparison ops (0x0A/0x0B) use raw field byte offsets rather than
    /// typed TSpecLinePtr params. Falls back to temp@hex if not a valid field offset.
    /// </summary>
    private string ResolveFieldOffset(int offset)
    {
        int fldSize = _run.Header.FieldSpecSize;
        if (fldSize > 0 && offset >= 0 && offset % fldSize == 0)
        {
            int idx = offset / fldSize;
            if (idx >= 0 && idx < _run.Fields.Count)
            {
                string fname = _run.Fields[idx].Name;
                if (!string.IsNullOrEmpty(fname) && fname.All(c => c >= ' ' && c <= '~'))
                    return fname;
                return $"FIELD[{idx}]";
            }
        }
        return $"temp@{offset:X}";
    }

    private string ResolveOperand(byte[] cs, int pos, Dictionary<int, string> temps)
    {
        if (pos + 5 > cs.Length) return "?";
        char type = (char)cs[pos];
        int loc = BitConverter.ToInt32(cs, pos + 1);

        // Check if this references a temp result from a previous operation
        if (type == 'F' && temps.ContainsKey(loc))
            return temps[loc];

        return _spec.ResolveParam(type, loc);
    }

    private static string GetBinaryOperator(byte op) => op switch
    {
        0x01 => "+",
        0x02 => "-",
        0x03 => "+",       // string concatenation (same symbol as add)
        0x04 => "*",
        0x05 => "/",
        0x06 => "^",
        0x07 => "=",
        0x08 => "<",
        0x09 => ">",
        0x0A => "<>",
        0x0B => ">=",
        0x0C => "<=",
        0x0D => " .AND. ",
        0x0E => " .OR. ",
        0x0F => " .NOT. ",
        _ => $"?{op:X2}?"
    };

    private static bool IsComparisonOp(byte op) =>
        op is 0x0D or 0x0E; // AND, OR — often need parens around sub-expressions

    private static bool ContainsBinaryOp(string expr) =>
        expr.Contains("=") || expr.Contains("<") || expr.Contains(">") ||
        expr.Contains(".AND.") || expr.Contains(".OR.");

    private static bool IsValidParamType(char c) =>
        c is 'F' or 'C' or 'N' or 'X' or 'x' or 'Y' or 'M' or 's' or 'q';

    private static bool IsStringConcat(string lhs, string rhs) =>
        lhs.StartsWith("'") || rhs.StartsWith("'") || lhs.Contains("'") || rhs.Contains("'");

    // TAS built-in function number → name mapping.
    // Reverse-engineered by cross-referencing compiled .RUN bytecode with original .SRC source files.
    private static readonly Dictionary<byte, string> _functionNames = new()
    {
        // System functions
        [0x01] = "date",
        [0x02] = "time",
        [0x0A] = "co",
        [0x36] = "prgnme",
        [0x94] = "prglne",
        [0x80] = "os",
        [0x9F] = "ver",

        // String functions
        [0x0B] = "len",
        [0x0C] = "justify",    // justify(field, alignment) — 'c'=center, 'r'=right, 'l'=left
        [0x0D] = "trc",        // trc(field, trimchar)
        [0x0E] = "up",
        [0x0F] = "low",
        [0x10] = "trim",
        [0x11] = "str",
        [0x12] = "chr",
        [0x13] = "asc",
        [0x14] = "isal",       // isal(field, position_or_type)
        [0x15] = "isup",
        [0x16] = "islo",
        [0x17] = "isnum",
        [0x18] = "null",
        [0x19] = "lstchr",
        [0x1A] = "sndx",
        [0x1C] = "mid",
        [0x1D] = "seg",
        [0x1E] = "loc",        // loc(needle, haystack)
        [0x23] = "iif",
        [0x24] = "replace",
        [0x27] = "diff",
        [0x2A] = "val",
        [0x2B] = "ccr",
        [0x2E] = "int",
        [0x8A] = "loc",        // alternate LOC entry? or different variant

        // Math functions
        [0x20] = "abs",
        [0x21] = "round",
        [0x22] = "sqrt",
        [0x25] = "sign",
        [0x26] = "mod",
        [0x28] = "ceil",
        [0x29] = "floor",
        [0x2C] = "rndm",
        [0x2D] = "pi",
        [0x2F] = "exp",
        [0x30] = "log",
        [0x31] = "log10",
        [0x32] = "sin",
        [0x33] = "cos",
        [0x34] = "tan",

        // Date/Time functions
        [0x1F] = "dtoc",
        [0x3C] = "date",       // date() system date as function — may overlap with 0x01
        [0x43] = "cdow",
        [0x44] = "dow",
        [0x45] = "dom",
        [0x46] = "mnth",
        [0x47] = "cmnth",
        [0x48] = "year",
        [0x49] = "dtos",
        [0x4A] = "ctod",
        [0x51] = "ttoc",
        [0x52] = "ttof",
        [0x53] = "ttor",
        [0x54] = "rtot",

        // File functions
        [0x35] = "flerr",
        [0x37] = "eof",
        [0x38] = "bof",
        [0x39] = "fnum",
        [0x3A] = "rcn",
        [0x3B] = "rsize",
        [0x3D] = "ftyp",
        [0x3E] = "fldnme",
        [0x3F] = "crec",
        [0x40] = "flsze",
        [0x41] = "numflds",
        [0x42] = "numlbls",
        [0x4B] = "tpath",
        [0x4C] = "cpath",
        [0x4D] = "dpath",     // alternate? or different variant from 0x64
        [0x4E] = "getenv",
        [0x5C] = "ffile",
        [0x5D] = "ffld",
        [0x5F] = "farray",
        [0x64] = "dpath",
        [0x69] = "open",

        // Conversion functions
        [0x55] = "dtor",
        [0x56] = "rtod",
        [0x57] = "cbyt",
        [0x58] = "cflt",
        [0x59] = "cint",
        [0x5A] = "ltoc",
        [0x5B] = "ctol",

        // Screen functions
        [0x78] = "row",
        [0x79] = "col",
        [0x7A] = "lrow",
        [0x7B] = "mcol",
        [0x7C] = "mrow",
        [0x7D] = "pcol",
        [0x7E] = "prow",

        // Runtime/UI functions
        [0x90] = "seg",        // seg(field, start, length, ...) — 4-arg variant
        [0xA0] = "avail",
        [0xA4] = "mem",
        [0xAD] = "esc",
        [0xAE] = "enter",
        [0xAF] = "inkey",
        [0xB0] = "varread",
        [0xB1] = "ask",
        [0xB6] = "rsize",     // alternate entry
        [0xB7] = "pstat",
        [0xBE] = "prop",
        [0xC2] = "exec",
        [0xC5] = "test",
    };

    private static string GetFunctionName(byte funcNum)
    {
        if (_functionNames.TryGetValue(funcNum, out var name))
            return name;
        return $"fn{funcNum:X2}";
    }
}
