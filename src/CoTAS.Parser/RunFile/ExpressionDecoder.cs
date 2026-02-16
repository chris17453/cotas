using System.Text;

namespace CoTAS.Parser.RunFile;

/// <summary>
/// Decodes compiled TAS expressions (RPN bytecode) back to infix notation.
/// Expression format in constant segment:
///   [type 'A'(1)] [dec(1)] [displaySize(2)] [0xFD(1)] [tempBase(4)] [operations...] [0xFF]
///
/// TAS 5.1 (TAS32 / old style):
///   Function: opType(1) + funcNum(1 byte) + args(5*N) + result(4)
///     opType = argCount + 1  (range 0x01-0x09, max 8 args)
///   UDF:      opType(1) + labelIdx(4) + tempBase(4) + args(5*N) + result(4)
///     opType = argCount + 10 (range 0x0A-0x13)
///
/// TAS 6.0+ (TASWN / new style):
///   Function: opType(1) + funcNum(2 bytes) + args(5*N) + result(4)
///     opType = argCount + 1  (range 0x01-0x13, max 18 args)
///   UDF:      opType(1) + labelIdx(4) + tempBase(4) + args(5*N) + result(4)
///     opType = argCount + 20 (range 0x14-0x2D)
///
/// Common:
///   0x00: binary op — 00 + operator(1) + [negate(1)?] lhs(5) + [negate(1)?] rhs(5) + result(4)
///   0xB4-0xB6: array element access (180-182)
///   0xFF: terminator
/// </summary>
public sealed class ExpressionDecoder
{
    private readonly RunFileReader _run;
    private readonly SpecDecoder _spec;
    private readonly bool _isTas51;
    private readonly int _udfStart;     // 10 for TAS 5.1, 20 for TAS 6.0+
    private readonly int _funcNumSize;  // 1 for TAS 5.1, 2 for TAS 6.0+

    public ExpressionDecoder(RunFileReader run, SpecDecoder spec)
    {
        _run = run;
        _spec = spec;
        _isTas51 = run.Header.ProType == "TAS32";
        _udfStart = _isTas51 ? 10 : 20;
        _funcNumSize = _isTas51 ? 1 : 2;
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
                // Format: 00 + operator(1) + [negate(1)?] + lhs(5) + [negate(1)?] + rhs(5) + result(4)
                // Negate flag = byte 99 (0x63) inserted before the type byte
                if (rpos + 16 > exprEnd) break;
                byte op = cs[rpos + 1];
                int p = rpos + 2;

                // Check for negate flag on lhs
                bool lhsNeg = false;
                if (p < exprEnd && cs[p] == 99) { lhsNeg = true; p++; }
                string lhs = ResolveOperand(cs, p, temps);
                p += 5;

                // Check for negate flag on rhs
                bool rhsNeg = false;
                if (p < exprEnd && cs[p] == 99) { rhsNeg = true; p++; }
                string rhs = ResolveOperand(cs, p, temps);
                p += 5;

                if (p + 4 > exprEnd) break;
                int resultOff = BitConverter.ToInt32(cs, p);

                if (lhsNeg) lhs = $"-{lhs}";
                if (rhsNeg) rhs = $"-{rhs}";

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
                rpos = p + 4;
            }
            else if (opType >= 0x01 && opType < _udfStart) // Built-in function call
            {
                // TAS 5.1: opType(1) + funcNum(1) + args(5*N) + result(4)
                // TAS 6.0+: opType(1) + funcNum(2) + args(5*N) + result(4)
                int headerSize = 1 + _funcNumSize;
                if (rpos + headerSize > exprEnd) break;

                int funcNum;
                if (_isTas51)
                    funcNum = cs[rpos + 1];
                else
                    funcNum = BitConverter.ToUInt16(cs, rpos + 1);

                int numArgs = opType - 1;

                var args = new List<string>();
                int argPos = rpos + headerSize;
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
            else if (opType >= _udfStart && opType < 180) // UDF call
            {
                // Format: opType(1) + labelIdx(4) + tempBase(4) + args(5*N) + result(4)
                if (rpos + 9 > exprEnd) break;
                int labelIdx = BitConverter.ToInt32(cs, rpos + 1);
                int numArgs = opType - _udfStart;
                int argPos = rpos + 9; // skip opType(1) + labelIdx(4) + tempBase(4)

                var args = new List<string>();
                for (int a = 0; a < numArgs; a++)
                {
                    if (argPos + 5 > exprEnd) break;
                    args.Add(ResolveOperand(cs, argPos, temps));
                    argPos += 5;
                }

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
            else if (opType >= 180 && opType <= 182) // Array element access (0xB4/B5/B6)
            {
                // [opType(1)] [element_typ(1)+element_loc(4)] [major_fld_loc(4)] [recv_fld_loc(4)] = 14 bytes
                if (rpos + 14 > exprEnd) break;
                string indexExpr = ResolveOperand(cs, rpos + 1, temps);
                int arrayFldLoc = BitConverter.ToInt32(cs, rpos + 6);
                int resultOff = BitConverter.ToInt32(cs, rpos + 10);

                string arrayField = ResolveFieldByOffset(arrayFldLoc);

                // opType 181 = macro array, 182 = indirect element
                string result = $"{arrayField}[{indexExpr}]";
                temps[resultOff] = result;
                lastResult = result;
                rpos += 14;
            }
            else
            {
                // Unknown operation type — bail
                break;
            }
        }

        return lastResult.Length > 0 ? lastResult : $"EXPR@{offset}";
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

    // Complete TAS built-in function table from cmpspec3.pas FunctionList[1..287].
    // Stored value = FunctionList index - 1 (0-based).
    private static readonly Dictionary<int, string> _functionNames = new()
    {
        [0] = "NOP",
        [1] = "DATE",
        [2] = "COL",
        [3] = "ROW",
        [4] = "PCOL",
        [5] = "PROW",
        [6] = "SQRT",
        [7] = "DSPCE",
        [8] = "INKEY",
        [9] = "TIME",
        [10] = "CO",
        [11] = "FILL",
        [12] = "JUST",
        [13] = "TRIM",
        [14] = "UP",
        [15] = "LOW",
        [16] = "PROP",
        [17] = "CHR",
        [18] = "ASC",
        [19] = "GFL",
        [20] = "SIZE",
        [21] = "FTYP",
        [22] = "DOW",
        [23] = "CDOW",
        [24] = "MNTH",
        [25] = "CMNTH",
        [26] = "YEAR",
        [27] = "LOC",
        [28] = "MID",
        [29] = "FFLD",
        [30] = "FARRAY",
        [31] = "DTOC",
        [32] = "CTOD",
        [33] = "DTOR",
        [34] = "RTOD",
        [35] = "IIF",
        [36] = "ISAL",
        [37] = "ISUP",
        [38] = "ISLO",
        [39] = "MAX",
        [40] = "MIN",
        [41] = "XFORM",
        [42] = "VAL",
        [43] = "STR",
        [44] = "RNDM",
        [45] = "ISCLR",
        [46] = "FLSZE",
        [47] = "EOF",
        [48] = "BOF",
        [49] = "LROW",
        [50] = "LCKD",
        [51] = "ESC",
        [52] = "GSCHR",
        [53] = "FLERR",
        [54] = "PWHR",
        [55] = "CFLT",
        [56] = "CINT",
        [57] = "CBYT",
        [58] = "CREC",
        [59] = "CCH",
        [60] = "CPATH",
        [61] = "IFCR",
        [62] = "RCN",
        [63] = "TRC",
        [64] = "PERR",
        [65] = "ALOCARY",
        [66] = "AVAIL",
        [67] = "ASK",
        [68] = "ETYP",
        [69] = "LSTCHR",
        [70] = "RENF",
        [71] = "WRAP",
        [72] = "IFNA",
        [73] = "LOG",
        [74] = "LOG10",
        [75] = "TPATH",
        [76] = "CCE",
        [77] = "FCHR",
        [78] = "CC",
        [79] = "PSTAT",
        [80] = "FNUM",
        [81] = "TTOC",
        [82] = "CTOT",
        [83] = "TTOR",
        [84] = "RTOT",
        [85] = "EXP",
        [86] = "CCF",
        [87] = "CCR",
        [88] = "FTOT",
        [89] = "TTOF",
        [90] = "RTP",
        [91] = "ALC_FLD",
        [92] = "FFILE",
        [93] = "PSET",
        [94] = "AEV",
        [95] = "LCHR",
        [96] = "MOD",
        [97] = "FLDATE",
        [98] = "DELF",
        [99] = "MID_REC",
        [100] = "DPATH",
        [101] = "DIFF",
        [102] = "SNDX",
        [103] = "PI",
        [104] = "ACOS",
        [105] = "ASIN",
        [106] = "ATAN",
        [107] = "COS",
        [108] = "SIN",
        [109] = "TAN",
        [110] = "ATAN2",
        [111] = "ABS",
        [112] = "CEIL",
        [113] = "FLOOR",
        [114] = "OS",
        [115] = "VER",
        [116] = "DMY",
        [117] = "DTOS",
        [118] = "NUMFLDS",
        [119] = "FLDNME",
        [120] = "GETENV",
        [121] = "INT",
        [122] = "LIKE",
        [123] = "MDY",
        [124] = "RSIZE",
        [125] = "MEM",
        [126] = "ROUND",
        [127] = "SIGN",
        [128] = "VARREAD",
        [129] = "NULL",
        [130] = "ELOC",
        [131] = "ENTER",
        [132] = "MOUSE_ACT",
        [133] = "ACS",
        [134] = "HEX",
        [135] = "LTOC",
        [136] = "CTOL",
        [137] = "CLNUM",
        [138] = "ALOC",
        [139] = "FLTIME",
        [140] = "WRAPO",
        [141] = "WRAPL",
        [142] = "WRAPS",
        [143] = "FLDFNUM",
        [144] = "GFLD",
        [145] = "SYSOPS",
        [146] = "CRSU",
        [147] = "EXEC",
        [148] = "OPEN",
        [149] = "DOM",
        [150] = "NUMLBLS",
        [151] = "LBLNME",
        [152] = "LBL_LNE",
        [153] = "PRGNME",
        [154] = "PRGLNE",
        [155] = "PRG_OFST",
        [156] = "MOUSE_ROW",
        [157] = "MOUSE_COL",
        [158] = "TEST",
        [159] = "ISNUM",
        [160] = "RETVAL",
        [161] = "TRAP_LABEL",
        [162] = "TRAP_DO",
        [163] = "REC_PTR",
        [164] = "GET_REC",
        [165] = "CUR_PRG",
        [166] = "RECORD_CHR",
        [167] = "OFFSET3",
        [168] = "MEMO_COL",
        [169] = "MEMO_ROW",
        [170] = "LISTF_CHRS",
        [171] = "MOUSE_ON",
        [172] = "SERIAL_PORT",
        [173] = "REC_CHANGED",
        [174] = "COMP_BIN",
        [175] = "ACTIVE",
        [176] = "WINDOWS",
        [177] = "PRINT_CANCEL",
        [178] = "PRINTER_NAME",
        [179] = "SROK",
        [180] = "DATE_TYPE",
        [181] = "DATE_SEPARATOR",
        [182] = "LOAD_OVL",
        [183] = "CHK_PSC",
        [184] = "LOAD_OBJ",
        [185] = "LPATH",
        [186] = "WINDOW_PTR",
        [187] = "MAKE_DIR",
        [188] = "COPY_FILE",
        [189] = "EDIT",
        [190] = "GET_WCOLOR",
        [191] = "REGEDIT",
        [192] = "SETUP_PRINTER",
        [193] = "CHOOSE_COLOR",
        [194] = "WQUIT",
        [195] = "GET_ELEM_NUM",
        [196] = "CLICKED_ON",
        [197] = "WLASER_PRT",
        [198] = "MAX_ROWS",
        [199] = "MAX_COLS",
        [200] = "DIRECT",
        [201] = "DSROK",
        [202] = "WHICH_SIZE",
        [203] = "NUM_USERS",
        [204] = "END_DATE",
        [205] = "GET_OBJ_PROP",
        [206] = "LOAD_MODAL_FORM",
        [207] = "REG_CODE",
        [208] = "GET_FORM_NAME",
        [209] = "GET_PATH",
        [210] = "RESET_FIELD",
        [211] = "STRINGS",
        [212] = "CREATE_DBF",
        [213] = "CONVERT_TO_DBF",
        [214] = "LOAD_PRG",
        [215] = "CALL_PRG",
        [216] = "FORM_PTR",
        [217] = "IS_PRG_LOADED",
        [218] = "REMOVE_PRG",
        [219] = "DUAL_LIST_EXEC",
        [220] = "FILE_EXISTS",
        [221] = "TEXT_WIDTH",
        [222] = "GET_RUN_PRG",
        [223] = "STATUS_BAR",
        [224] = "GET_FILE",
        [225] = "XPATH",
        [226] = "WHOAMI",
        [227] = "PARSEFILE",
        [228] = "OPEN_FILE_NAME",
        [229] = "MAKE_PATH",
        [230] = "LAST_FILE",
        [231] = "LAST_OBJ",
        [232] = "VALID_CHECK",
        [233] = "GRID_VALID_CHECK",
        [234] = "SET_OBJ_PROP",
        [235] = "SETENV",
        [236] = "USECODEBASE",
        [237] = "CRLF",
        [238] = "GET_COMP_NAME",
        [239] = "GET_USER_NAME",
        [240] = "GET_BUFF_NAME",
        [241] = "REC_PERCENTAGE",
        [242] = "GET_INI_PATH",
        [243] = "EMAIL",
        [244] = "TLLFC",
        [245] = "RESTRUCTURE_DBF",
        [246] = "IFDUPCB",
        [247] = "DFM_TO_TXT",
        [248] = "TXT_TO_DFM",
        [249] = "ADD_OBJECT",
        [250] = "ENCRYPT",
        [251] = "PACK_DBF",
        [252] = "REINDEX_DBF",
        [253] = "LOAD_DLL",
        [254] = "DLLFC",
        [255] = "REMOVE_DLL",
        [256] = "COMPILE_EXPR",
        [257] = "COMPILE_SRC",
        [258] = "LICENSE",
        [259] = "REC_LOCK",
        [260] = "FILE_STORE",
        [261] = "LAST_OBJ_CLICK",
        [262] = "REPORT_LINES",
        [263] = "GET_KEY_NUM",
        [264] = "REPLACE",
        [265] = "FAX",
        [266] = "FILE_TYPE",
        [267] = "DELETED",
        [268] = "WIDGET",
        [269] = "CHANGE_SKIN",
        [270] = "WAIT_CHECK",
        [271] = "REC_DEL",
        [272] = "SQL",
        [273] = "FILE_ATTRIB",
        [274] = "DIR_EXISTS",
        [275] = "ABOUT",
        [276] = "CLR_TEMP_MEMORY",
        [277] = "CREATE_BTRV",
        [278] = "X_CHARGE",
        [279] = "ENCRYPTSTR",
        [280] = "DECRYPTSTR",
        [281] = "PROGLINE",
        [282] = "IS_LEAP_YEAR",
        [283] = "DAYS_IN_MONTH",
        [284] = "INC_MONTH",
        [285] = "HASHFILE",
        [286] = "LOCKEDUSER",
    };

    private string ResolveFieldByOffset(int offset)
    {
        int fldSize = _run.Header.FieldSpecSize;
        if (fldSize > 0 && offset >= 0)
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
        return $"FLD@{offset}";
    }

    private static string GetFunctionName(int funcNum)
    {
        if (_functionNames.TryGetValue(funcNum, out var name))
            return name;
        return $"fn{funcNum}";
    }
}
