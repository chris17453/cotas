using System.Diagnostics;

namespace CoTAS.Interpreter.Functions;

/// <summary>
/// System functions: GETENV, OS, VER, PRGNME, PRGLNE, VARREAD, ESC, ENTER, INKEY, EXEC,
/// PSTAT, AVAIL, DSPCE, MEM, CO, ETYP, ASK
/// These functions access the FieldManager's system state properties.
/// </summary>
public static class SystemFunctions
{
    // We need a reference to the FieldManager for system state access
    private static FieldManager? _fields;

    public static void Register(Dictionary<string, Func<List<TasValue>, TasValue>> registry, FieldManager? fields = null)
    {
        _fields = fields;
        registry["GETENV"] = GetEnv;
        registry["OS"] = Os;
        registry["VER"] = Ver;
        registry["PRGNME"] = PrgNme;
        registry["PRGLNE"] = PrgLne;
        registry["VARREAD"] = VarRead;
        registry["ESC"] = Esc;
        registry["ENTER"] = Enter;
        registry["INKEY"] = InKey;
        registry["EXEC"] = Exec;
        registry["PSTAT"] = PStat;
        registry["AVAIL"] = Avail;
        registry["DSPCE"] = DSpce;
        registry["MEM"] = Mem;
        registry["CO"] = Co;
        registry["ETYP"] = ETyp;
        registry["ASK"] = AskFunc;
    }

    /// <summary>Set the FieldManager reference for state access.</summary>
    public static void SetFieldManager(FieldManager fields) => _fields = fields;

    private static TasValue GetEnv(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("GETENV() requires 1 argument");
        string val = Environment.GetEnvironmentVariable(args[0].AsString().Trim()) ?? "";
        return new TasValue(TasType.Alpha, val, val.Length);
    }

    private static TasValue Os(List<TasValue> args)
    {
        return new TasValue(TasType.Alpha, "COTAS", 10);
    }

    private static TasValue Ver(List<TasValue> args)
    {
        return new TasValue(TasType.Alpha, "2.0", 10);
    }

    private static TasValue PrgNme(List<TasValue> args)
    {
        string name = _fields?.ProgramName ?? "PROGRAM";
        return new TasValue(TasType.Alpha, name, 20);
    }

    private static TasValue PrgLne(List<TasValue> args)
    {
        int line = _fields?.ProgramLine ?? 0;
        return new TasValue(TasType.Integer, line);
    }

    private static TasValue VarRead(List<TasValue> args)
    {
        string name = _fields?.LastVarRead ?? "";
        return new TasValue(TasType.Alpha, name, 20);
    }

    private static TasValue Esc(List<TasValue> args)
    {
        bool pressed = _fields?.LastEscPressed ?? false;
        return new TasValue(TasType.Logical, pressed);
    }

    private static TasValue Enter(List<TasValue> args)
    {
        bool pressed = _fields?.LastEnterPressed ?? true;
        return new TasValue(TasType.Logical, pressed);
    }

    private static TasValue InKey(List<TasValue> args)
    {
        int key = _fields?.LastKeyPressed ?? 0;
        return new TasValue(TasType.Integer, key);
    }

    private static TasValue Exec(List<TasValue> args)
    {
        // EXEC(command) - execute OS command and return exit code
        if (args.Count < 1) return new TasValue(TasType.Integer, 0);
        string command = args[0].AsString().Trim();
        if (string.IsNullOrEmpty(command)) return new TasValue(TasType.Integer, 0);
        try
        {
            var psi = new ProcessStartInfo
            {
                FileName = OperatingSystem.IsWindows() ? "cmd" : "/bin/sh",
                Arguments = OperatingSystem.IsWindows() ? $"/c {command}" : $"-c \"{command}\"",
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                CreateNoWindow = true,
            };
            using var process = Process.Start(psi);
            process?.WaitForExit(30000); // 30 second timeout
            return new TasValue(TasType.Integer, process?.ExitCode ?? -1);
        }
        catch
        {
            return new TasValue(TasType.Integer, -1);
        }
    }

    private static TasValue PStat(List<TasValue> args)
    {
        // PSTAT() - printer status: 0 = ready
        bool on = _fields?.PrinterOn ?? false;
        return new TasValue(TasType.Integer, on ? 0 : 1);
    }

    private static TasValue Avail(List<TasValue> args)
    {
        // AVAIL() - available disk space in bytes
        try
        {
            var drive = new DriveInfo(Directory.GetCurrentDirectory());
            return new TasValue(TasType.Integer, (int)Math.Min(drive.AvailableFreeSpace, int.MaxValue));
        }
        catch
        {
            return new TasValue(TasType.Integer, 999999999);
        }
    }

    private static TasValue DSpce(List<TasValue> args)
    {
        // DSPCE() - disk space (same as AVAIL)
        return Avail(args);
    }

    private static TasValue Mem(List<TasValue> args)
    {
        // MEM() - available memory
        long mem = GC.GetTotalMemory(false);
        long avail = Environment.WorkingSet - mem;
        return new TasValue(TasType.Integer, (int)Math.Max(avail, 999999));
    }

    private static TasValue Co(List<TasValue> args)
    {
        string code = _fields?.CompanyCode ?? "01";
        return new TasValue(TasType.Alpha, code, 2);
    }

    private static TasValue ETyp(List<TasValue> args)
    {
        int errType = _fields?.LastErrorType ?? 0;
        return new TasValue(TasType.Integer, errType);
    }

    private static TasValue AskFunc(List<TasValue> args)
    {
        // ASK() function - returns result of last ASK command
        bool result = _fields?.LastAskResult ?? true;
        return new TasValue(TasType.Logical, result);
    }
}
