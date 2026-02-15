namespace CoTAS.Interpreter.Functions;

/// <summary>
/// File functions: EOF, BOF, FLERR, FNUM, RCN, CREC, RSIZE, OPEN, FTYP, FLDNME, etc.
/// These query the FieldManager for file handle state stored as __FILE_* system variables.
/// File handles set these variables during OPENV/FINDV/SAVE/etc operations.
/// </summary>
public static class FileFunctions
{
    private static FieldManager? _fields;

    public static void Register(Dictionary<string, Func<List<TasValue>, TasValue>> registry, FieldManager? fields = null)
    {
        _fields = fields;
        registry["EOF"] = Eof;
        registry["BOF"] = Bof;
        registry["FLERR"] = FlErr;
        registry["FNUM"] = FNum;
        registry["RCN"] = Rcn;
        registry["CREC"] = Crec;
        registry["RSIZE"] = RSize;
        registry["OPEN"] = Open;
        registry["FTYP"] = FTyp;
        registry["FLDNME"] = FldNme;
        registry["FLSZE"] = FlSze;
        registry["FLDFDNUM"] = FldFdNum;
        registry["NUMFLDS"] = NumFlds;
        registry["NUMLBLS"] = NumLbls;
        registry["LBLNME"] = LblNme;
        registry["LCKD"] = Lckd;
        registry["IFNA"] = IfNa;
        registry["IFCR"] = IfCr;
        registry["PERR"] = PErr;
        registry["FFILE"] = FFile;
        registry["FFLD"] = FFld;
        registry["FARRAY"] = FArray;
        registry["FTOT"] = FTot;
        registry["GFL"] = Gfl;
        registry["DELF"] = DelF;
        registry["RENF"] = RenF;
        registry["CPATH"] = CPath;
        registry["DPATH"] = DPath;
        registry["TPATH"] = TPath;
        registry["PROP"] = Prop;
        registry["PSET"] = PSet;
        registry["TEST"] = Test;
    }

    public static void SetFieldManager(FieldManager fields) => _fields = fields;

    /// <summary>Get a system variable from FieldManager, returning default if not found.</summary>
    private static TasValue GetSysVar(string name, TasValue defaultValue)
    {
        if (_fields == null) return defaultValue;
        try { return _fields.Get(name); }
        catch { return defaultValue; }
    }

    /// <summary>Get file handle number from first argument.</summary>
    private static int GetHandle(List<TasValue> args) =>
        args.Count > 0 ? args[0].AsInteger() : 0;

    private static TasValue Eof(List<TasValue> args)
    {
        // EOF(handle) - check if at end of file
        int h = GetHandle(args);
        return GetSysVar($"__FILE_{h}_EOF", new TasValue(TasType.Logical, true));
    }

    private static TasValue Bof(List<TasValue> args)
    {
        // BOF(handle) - check if at beginning of file
        int h = GetHandle(args);
        return GetSysVar($"__FILE_{h}_BOF", new TasValue(TasType.Logical, true));
    }

    private static TasValue FlErr(List<TasValue> args)
    {
        // FLERR(handle) - get file error code (0 = no error)
        int h = GetHandle(args);
        return GetSysVar($"__FILE_{h}_ERR", new TasValue(TasType.Integer, 0));
    }

    private static TasValue FNum(List<TasValue> args)
    {
        // FNUM(name) - get file number for a named file
        if (args.Count < 1) return new TasValue(TasType.Integer, 0);
        string name = args[0].AsString().Trim().ToUpper();
        return GetSysVar($"__FILE_NUM_{name}", new TasValue(TasType.Integer, 0));
    }

    private static TasValue Rcn(List<TasValue> args)
    {
        // RCN(handle) - get current record number
        int h = GetHandle(args);
        return GetSysVar($"__FILE_{h}_RCN", new TasValue(TasType.Integer, 0));
    }

    private static TasValue Crec(List<TasValue> args)
    {
        // CREC(handle) - get current record number (alias for RCN)
        return Rcn(args);
    }

    private static TasValue RSize(List<TasValue> args)
    {
        // RSIZE(handle) - get record size
        int h = GetHandle(args);
        return GetSysVar($"__FILE_{h}_RSIZE", new TasValue(TasType.Integer, 0));
    }

    private static TasValue Open(List<TasValue> args)
    {
        // OPEN(handle) - check if file handle is open
        int h = GetHandle(args);
        return GetSysVar($"__FILE_{h}_OPEN", new TasValue(TasType.Logical, false));
    }

    private static TasValue FTyp(List<TasValue> args)
    {
        // FTYP(handle, fieldnum) - get field type
        int h = args.Count > 0 ? args[0].AsInteger() : 0;
        return GetSysVar($"__FILE_{h}_FTYP", new TasValue(TasType.Alpha, "A", 1));
    }

    private static TasValue FldNme(List<TasValue> args)
    {
        // FLDNME(handle, fieldnum) - get field name by number
        int h = args.Count > 0 ? args[0].AsInteger() : 0;
        int num = args.Count > 1 ? args[1].AsInteger() : 0;
        return GetSysVar($"__FILE_{h}_FLDNME_{num}", new TasValue(TasType.Alpha, "", 20));
    }

    private static TasValue FlSze(List<TasValue> args)
    {
        // FLSZE(handle) - get file size (number of records)
        int h = GetHandle(args);
        return GetSysVar($"__FILE_{h}_SIZE", new TasValue(TasType.Integer, 0));
    }

    private static TasValue FldFdNum(List<TasValue> args)
    {
        // FLDFDNUM(handle, fieldname) - get field number by name
        int h = args.Count > 0 ? args[0].AsInteger() : 0;
        string name = args.Count > 1 ? args[1].AsString().Trim() : "";
        return GetSysVar($"__FILE_{h}_FDNUM_{name}", new TasValue(TasType.Integer, 0));
    }

    private static TasValue NumFlds(List<TasValue> args)
    {
        // NUMFLDS(handle) - get number of fields in file
        int h = GetHandle(args);
        return GetSysVar($"__FILE_{h}_NUMFLDS", new TasValue(TasType.Integer, 0));
    }

    private static TasValue NumLbls(List<TasValue> args)
    {
        // NUMLBLS() - get number of labels in current program
        if (_fields == null) return new TasValue(TasType.Integer, 0);
        return GetSysVar("__NUMLBLS", new TasValue(TasType.Integer, 0));
    }

    private static TasValue LblNme(List<TasValue> args)
    {
        // LBLNME(index) - get label name by index
        int idx = args.Count > 0 ? args[0].AsInteger() : 0;
        return GetSysVar($"__LBLNME_{idx}", new TasValue(TasType.Alpha, "", 20));
    }

    private static TasValue Lckd(List<TasValue> args)
    {
        // LCKD(handle) - check if current record is locked
        int h = GetHandle(args);
        return GetSysVar($"__FILE_{h}_LOCKED", new TasValue(TasType.Logical, false));
    }

    private static TasValue IfNa(List<TasValue> args)
    {
        // IFNA(handle) - check if record is not active (deleted)
        int h = GetHandle(args);
        return GetSysVar($"__FILE_{h}_INACTIVE", new TasValue(TasType.Logical, false));
    }

    private static TasValue IfCr(List<TasValue> args)
    {
        // IFCR(handle) - check if record is the current record
        int h = GetHandle(args);
        return GetSysVar($"__FILE_{h}_ISCURRENT", new TasValue(TasType.Logical, false));
    }

    private static TasValue PErr(List<TasValue> args)
    {
        // PERR() - get program error code
        if (_fields == null) return new TasValue(TasType.Integer, 0);
        return GetSysVar("__PERR", new TasValue(TasType.Integer, 0));
    }

    private static TasValue FFile(List<TasValue> args)
    {
        // FFILE(handle) - get file name for handle
        int h = GetHandle(args);
        return GetSysVar($"__FILE_{h}_NAME", new TasValue(TasType.Alpha, "", 20));
    }

    private static TasValue FFld(List<TasValue> args)
    {
        // FFLD(handle) - get first field name in file
        int h = GetHandle(args);
        return GetSysVar($"__FILE_{h}_FLDNME_1", new TasValue(TasType.Alpha, "", 20));
    }

    private static TasValue FArray(List<TasValue> args)
    {
        // FARRAY(name) - get array size (0 if not an array)
        if (args.Count < 1) return new TasValue(TasType.Integer, 0);
        string name = args[0].AsString().Trim();
        int size = _fields?.GetArraySize(name) ?? 0;
        return new TasValue(TasType.Integer, size);
    }

    private static TasValue FTot(List<TasValue> args)
    {
        // FTOT(handle) - get file total records
        int h = GetHandle(args);
        return GetSysVar($"__FILE_{h}_TOTAL", new TasValue(TasType.Numeric, 0.0));
    }

    private static TasValue Gfl(List<TasValue> args)
    {
        // GFL(handle, fieldname) - get field value from file handle
        // Returns the string value of a field in a file
        int h = args.Count > 0 ? args[0].AsInteger() : 0;
        string fieldName = args.Count > 1 ? args[1].AsString().Trim() : "";
        return GetSysVar($"__FILE_{h}_GFL_{fieldName}", new TasValue(TasType.Alpha, "", 80));
    }

    private static TasValue DelF(List<TasValue> args)
    {
        // DELF(filename) - delete a file from disk
        if (args.Count < 1) return new TasValue(TasType.Logical, false);
        string path = args[0].AsString().Trim();
        try
        {
            if (File.Exists(path))
            {
                File.Delete(path);
                return new TasValue(TasType.Logical, true);
            }
            return new TasValue(TasType.Logical, false);
        }
        catch
        {
            return new TasValue(TasType.Logical, false);
        }
    }

    private static TasValue RenF(List<TasValue> args)
    {
        // RENF(oldname, newname) - rename a file
        if (args.Count < 2) return new TasValue(TasType.Logical, false);
        string oldPath = args[0].AsString().Trim();
        string newPath = args[1].AsString().Trim();
        try
        {
            if (File.Exists(oldPath))
            {
                File.Move(oldPath, newPath);
                return new TasValue(TasType.Logical, true);
            }
            return new TasValue(TasType.Logical, false);
        }
        catch
        {
            return new TasValue(TasType.Logical, false);
        }
    }

    private static TasValue CPath(List<TasValue> args)
    {
        // CPATH() - current working directory path
        string path = Directory.GetCurrentDirectory();
        return new TasValue(TasType.Alpha, path, 80);
    }

    private static TasValue DPath(List<TasValue> args)
    {
        // DPATH() - data path (uses MOUNT path if set)
        if (_fields != null)
        {
            try
            {
                var mp = _fields.Get("__MOUNT_PATH");
                string s = mp.AsString().Trim();
                if (!string.IsNullOrEmpty(s))
                    return new TasValue(TasType.Alpha, s, 80);
            }
            catch { /* no mount path */ }
        }
        string path = Directory.GetCurrentDirectory();
        return new TasValue(TasType.Alpha, path, 80);
    }

    private static TasValue TPath(List<TasValue> args)
    {
        // TPATH() - temp path
        string path = Path.GetTempPath();
        return new TasValue(TasType.Alpha, path, 80);
    }

    private static TasValue Prop(List<TasValue> args)
    {
        // PROP(name) - get property value
        if (args.Count < 1) return new TasValue(TasType.Alpha, "", 80);
        string propName = args[0].AsString().Trim().ToUpper();
        return propName switch
        {
            "OS" => new TasValue(TasType.Alpha, Environment.OSVersion.ToString(), 80),
            "MACHINE" => new TasValue(TasType.Alpha, Environment.MachineName, 80),
            "USER" => new TasValue(TasType.Alpha, Environment.UserName, 80),
            "VERSION" => new TasValue(TasType.Alpha, "2.0", 80),
            _ => GetSysVar($"__PROP_{propName}", new TasValue(TasType.Alpha, "", 80)),
        };
    }

    private static TasValue PSet(List<TasValue> args)
    {
        // PSET(name, value) - set property value
        if (args.Count < 2) return new TasValue(TasType.Logical, false);
        string propName = args[0].AsString().Trim().ToUpper();
        _fields?.Set($"__PROP_{propName}", args[1]);
        return new TasValue(TasType.Logical, true);
    }

    private static TasValue Test(List<TasValue> args)
    {
        // TEST(filename) - test if file exists
        if (args.Count < 1) return new TasValue(TasType.Logical, false);
        string path = args[0].AsString().Trim();
        return new TasValue(TasType.Logical, File.Exists(path) || Directory.Exists(path));
    }
}
