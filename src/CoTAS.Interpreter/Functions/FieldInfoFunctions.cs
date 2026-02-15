namespace CoTAS.Interpreter.Functions;

/// <summary>
/// Field information/manipulation functions with real implementations.
/// </summary>
public static class FieldInfoFunctions
{
    private static FieldManager? _fields;

    public static void Register(Dictionary<string, Func<List<TasValue>, TasValue>> registry, FieldManager? fields = null)
    {
        _fields = fields;
        registry["SIZE"] = Size;
        registry["OFST"] = Ofst;
        registry["JUST"] = Just;
        registry["ALC_FLD"] = AlcFld;
        registry["ALOC"] = Aloc;
        registry["ALOCARY"] = AlocAry;
        registry["GET_ELEM_NUM"] = GetElemNum;
        registry["AEV"] = Aev;
        registry["RTP"] = Rtp;
        registry["RECORD_CHR"] = RecordChr;
        registry["MID_REC"] = MidRec;
        registry["REC_PTR"] = RecPtr;
        registry["LBL_LNE"] = LblLne;
        registry["ELOC"] = ELoc;
        registry["FCHR"] = FChr;
        registry["LCHR"] = LChr;
        registry["LIKE"] = Like;
        registry["MIN"] = Min;
        registry["MAX"] = Max;
        registry["DMY"] = Dmy;
        registry["COPY_FILE"] = CopyFile;
        registry["MAKE_DIR"] = MakeDir;
        registry["PRINT_CANCEL"] = PrintCancel;
        registry["PRINTER_NAME"] = PrinterName;
        registry["LISTF_CHRS"] = ListfChrs;
        registry["CC"] = Cc;
        registry["CCE"] = Cce;
        registry["CCF"] = Ccf;
        registry["CCH"] = Cch;
        registry["RETVAL"] = RetVal;
    }

    public static void SetFieldManager(FieldManager fields) => _fields = fields;

    private static TasValue Size(List<TasValue> args)
    {
        if (args.Count < 1) return new TasValue(TasType.Integer, 0);
        return new TasValue(TasType.Integer, args[0].Size > 0 ? args[0].Size : args[0].AsString().Length);
    }

    private static TasValue Ofst(List<TasValue> args)
    {
        // OFST(fieldname) - offset in record buffer (not meaningful without file I/O; return 0)
        return new TasValue(TasType.Integer, 0);
    }

    private static TasValue Just(List<TasValue> args)
    {
        if (args.Count < 2) return args.Count > 0 ? args[0] : new TasValue(TasType.Alpha, "", 0);
        string s = args[0].AsString();
        int size = args[1].AsInteger();
        string align = args.Count >= 3 ? args[2].AsString().Trim().ToUpper() : "L";
        string result = align switch
        {
            "R" => s.TrimEnd().PadLeft(size),
            "C" => s.Trim().PadLeft((size + s.Trim().Length) / 2).PadRight(size),
            _ => s.TrimEnd().PadRight(size),
        };
        if (result.Length > size) result = result[..size];
        return new TasValue(TasType.Alpha, result, size);
    }

    private static TasValue AlcFld(List<TasValue> args)
    {
        // ALC_FLD(name, type, size) - allocate a field dynamically
        if (args.Count < 3 || _fields == null)
            return new TasValue(TasType.Logical, false);
        string name = args[0].AsString().Trim();
        string type = args[1].AsString().Trim();
        int size = args[2].AsInteger();
        _fields.Define(name, type, size, 0, 0);
        return new TasValue(TasType.Logical, true);
    }

    private static TasValue Aloc(List<TasValue> args)
    {
        // ALOC(name, type, size) - allocate memory field (same as ALC_FLD)
        return AlcFld(args);
    }

    private static TasValue AlocAry(List<TasValue> args)
    {
        // ALOCARY(name, type, size, count) - allocate array dynamically
        if (args.Count < 4 || _fields == null)
            return new TasValue(TasType.Logical, false);
        string name = args[0].AsString().Trim();
        string type = args[1].AsString().Trim();
        int size = args[2].AsInteger();
        int count = args[3].AsInteger();
        _fields.Define(name, type, size, 0, count);
        return new TasValue(TasType.Logical, true);
    }

    private static TasValue GetElemNum(List<TasValue> args)
    {
        // GET_ELEM_NUM(arrayname) - get number of elements in array
        if (args.Count < 1 || _fields == null) return new TasValue(TasType.Integer, 0);
        string name = args[0].AsString().Trim();
        return new TasValue(TasType.Integer, _fields.GetArraySize(name));
    }

    private static TasValue Aev(List<TasValue> args)
    {
        // AEV(arrayname, index) - get array element value as string
        if (args.Count < 2 || _fields == null) return new TasValue(TasType.Alpha, "", 20);
        string name = args[0].AsString().Trim();
        int idx = args[1].AsInteger();
        try
        {
            var val = _fields.GetArrayElement(name, idx);
            return new TasValue(TasType.Alpha, val.AsString(), 20);
        }
        catch
        {
            return new TasValue(TasType.Alpha, "", 20);
        }
    }

    private static TasValue Rtp(List<TasValue> args)
    {
        // RTP(handle) - get record type
        if (args.Count < 1) return new TasValue(TasType.Alpha, "", 1);
        int h = args[0].AsInteger();
        if (_fields == null) return new TasValue(TasType.Alpha, "", 1);
        try { return _fields.Get($"__FILE_{h}_RTP"); }
        catch { return new TasValue(TasType.Alpha, "", 1); }
    }

    private static TasValue RecordChr(List<TasValue> args)
    {
        // RECORD_CHR(handle, offset) - get character from record buffer at offset
        if (args.Count < 2) return new TasValue(TasType.Alpha, "", 1);
        int h = args[0].AsInteger();
        int offset = args[1].AsInteger();
        if (_fields == null) return new TasValue(TasType.Alpha, "", 1);
        try
        {
            var buf = _fields.Get($"__FILE_{h}_BUFFER");
            string s = buf.AsString();
            if (offset >= 1 && offset <= s.Length)
                return new TasValue(TasType.Alpha, s[offset - 1].ToString(), 1);
        }
        catch { /* no buffer */ }
        return new TasValue(TasType.Alpha, "", 1);
    }

    private static TasValue MidRec(List<TasValue> args)
    {
        // MID_REC(handle, offset, length) - get substring from record buffer
        if (args.Count < 3) return new TasValue(TasType.Alpha, "", 20);
        int h = args[0].AsInteger();
        int offset = args[1].AsInteger() - 1;
        int len = args[2].AsInteger();
        if (_fields == null) return new TasValue(TasType.Alpha, "", len);
        try
        {
            var buf = _fields.Get($"__FILE_{h}_BUFFER");
            string s = buf.AsString();
            if (offset >= 0 && offset < s.Length)
            {
                if (offset + len > s.Length) len = s.Length - offset;
                string result = s.Substring(offset, len);
                return new TasValue(TasType.Alpha, result, len);
            }
        }
        catch { /* no buffer */ }
        return new TasValue(TasType.Alpha, new string(' ', len), len);
    }

    private static TasValue RecPtr(List<TasValue> args)
    {
        // REC_PTR(handle) - get current record pointer position
        if (args.Count < 1) return new TasValue(TasType.Integer, 0);
        int h = args[0].AsInteger();
        if (_fields == null) return new TasValue(TasType.Integer, 0);
        try { return _fields.Get($"__FILE_{h}_RCN"); }
        catch { return new TasValue(TasType.Integer, 0); }
    }

    private static TasValue LblLne(List<TasValue> args)
    {
        // LBL_LNE(labelname) - get line number of label
        if (args.Count < 1) return new TasValue(TasType.Integer, 0);
        string name = args[0].AsString().Trim();
        if (_fields == null) return new TasValue(TasType.Integer, 0);
        try { return _fields.Get($"__LBL_LINE_{name}"); }
        catch { return new TasValue(TasType.Integer, 0); }
    }

    private static TasValue ELoc(List<TasValue> args)
    {
        if (args.Count < 2) return new TasValue(TasType.Integer, 0);
        string s = args[0].AsString();
        string ch = args[1].AsString();
        if (ch.Length == 0) return new TasValue(TasType.Integer, 0);
        int pos = s.LastIndexOf(ch[0]);
        return new TasValue(TasType.Integer, pos >= 0 ? pos + 1 : 0);
    }

    private static TasValue FChr(List<TasValue> args)
    {
        if (args.Count < 1) return new TasValue(TasType.Integer, 0);
        string s = args[0].AsString();
        for (int i = 0; i < s.Length; i++)
            if (s[i] != ' ') return new TasValue(TasType.Integer, i + 1);
        return new TasValue(TasType.Integer, 0);
    }

    private static TasValue LChr(List<TasValue> args)
    {
        if (args.Count < 1) return new TasValue(TasType.Integer, 0);
        string s = args[0].AsString();
        int pos = s.TrimEnd().Length;
        return new TasValue(TasType.Integer, pos);
    }

    private static TasValue Like(List<TasValue> args)
    {
        if (args.Count < 2) return new TasValue(TasType.Logical, false);
        string s = args[0].AsString().TrimEnd();
        string pattern = args[1].AsString().TrimEnd();
        bool result = WildcardMatch(s, pattern, 0, 0);
        return new TasValue(TasType.Logical, result);
    }

    private static bool WildcardMatch(string s, string p, int si, int pi)
    {
        while (pi < p.Length)
        {
            if (p[pi] == '*')
            {
                pi++;
                for (int i = si; i <= s.Length; i++)
                    if (WildcardMatch(s, p, i, pi)) return true;
                return false;
            }
            if (si >= s.Length) return false;
            if (p[pi] != '?' && !char.ToUpper(p[pi]).Equals(char.ToUpper(s[si])))
                return false;
            si++;
            pi++;
        }
        return si == s.Length;
    }

    private static TasValue Min(List<TasValue> args)
    {
        if (args.Count < 2) throw new InterpreterException("MIN() requires 2 arguments");
        double a = args[0].AsNumeric();
        double b = args[1].AsNumeric();
        return new TasValue(TasType.Numeric, Math.Min(a, b));
    }

    private static TasValue Max(List<TasValue> args)
    {
        if (args.Count < 2) throw new InterpreterException("MAX() requires 2 arguments");
        double a = args[0].AsNumeric();
        double b = args[1].AsNumeric();
        return new TasValue(TasType.Numeric, Math.Max(a, b));
    }

    private static TasValue Dmy(List<TasValue> args)
    {
        if (args.Count < 1) return new TasValue(TasType.Alpha, "", 18);
        var dt = DateTimeFunctions.ParseDate(args[0].AsString());
        if (dt == null) return new TasValue(TasType.Alpha, "", 18);
        string s = dt.Value.ToString("dd MMMM yyyy");
        return new TasValue(TasType.Alpha, s, 18);
    }

    private static TasValue CopyFile(List<TasValue> args)
    {
        // COPY_FILE(source, dest) - actually copy the file
        if (args.Count < 2) return new TasValue(TasType.Logical, false);
        string src = args[0].AsString().Trim();
        string dst = args[1].AsString().Trim();
        try
        {
            File.Copy(src, dst, true);
            return new TasValue(TasType.Logical, true);
        }
        catch
        {
            return new TasValue(TasType.Logical, false);
        }
    }

    private static TasValue MakeDir(List<TasValue> args)
    {
        // MAKE_DIR(path) - actually create directory
        if (args.Count < 1) return new TasValue(TasType.Logical, false);
        string path = args[0].AsString().Trim();
        try
        {
            Directory.CreateDirectory(path);
            return new TasValue(TasType.Logical, true);
        }
        catch
        {
            return new TasValue(TasType.Logical, false);
        }
    }

    private static TasValue PrintCancel(List<TasValue> args) =>
        new(TasType.Logical, false); // print cancel is a UI concept

    private static TasValue PrinterName(List<TasValue> args) =>
        new(TasType.Alpha, "DEFAULT", 20); // always default in web mode

    private static TasValue ListfChrs(List<TasValue> args) =>
        new(TasType.Alpha, "", 80); // list field chars (UI concept)

    private static TasValue Cc(List<TasValue> args)
    {
        // CC() - control character (carriage return)
        return new TasValue(TasType.Alpha, "\r", 1);
    }

    private static TasValue Cce(List<TasValue> args)
    {
        // CCE() - escape character
        return new TasValue(TasType.Alpha, "\x1B", 1);
    }

    private static TasValue Ccf(List<TasValue> args)
    {
        // CCF() - form feed character
        return new TasValue(TasType.Alpha, "\f", 1);
    }

    private static TasValue Cch(List<TasValue> args)
    {
        // CCH() - horizontal tab character
        return new TasValue(TasType.Alpha, "\t", 1);
    }

    private static TasValue RetVal(List<TasValue> args)
    {
        // RETVAL() - return value from last UDF call
        var rv = _fields?.LastReturnValue;
        return rv ?? new TasValue(TasType.Alpha, "", 0);
    }
}
