using CoTAS.Parser.Ast;
using CoTAS.Interpreter.Functions;

namespace CoTAS.Interpreter;

public sealed class ExpressionEvaluator
{
    private readonly FieldManager _fields;
    private readonly Dictionary<string, Func<List<TasValue>, TasValue>> _functions = new(StringComparer.OrdinalIgnoreCase);

    /// <summary>
    /// Callback to the interpreter for executing User Defined Functions (UDFs).
    /// Returns null if the name is not a UDF.
    /// </summary>
    public Func<string, List<TasValue>, TasValue?>? UdfCallback { get; set; }

    public ExpressionEvaluator(FieldManager fields)
    {
        _fields = fields;
        RegisterBuiltinFunctions();
    }

    private void RegisterBuiltinFunctions()
    {
        // Original built-ins
        _functions["TRIM"] = BuiltinTrim;
        _functions["TRC"] = BuiltinTrim;
        _functions["UP"] = BuiltinUp;
        _functions["LOW"] = BuiltinLow;
        _functions["STR"] = BuiltinStr;
        _functions["VAL"] = BuiltinVal;
        _functions["MID"] = BuiltinMid;
        _functions["LEN"] = BuiltinLen;
        _functions["CHR"] = BuiltinChr;
        _functions["ASC"] = BuiltinAsc;
        _functions["ABS"] = BuiltinAbs;
        _functions["INT"] = BuiltinInt;
        _functions["CLNUM"] = BuiltinClnum;
        _functions["CINT"] = BuiltinCint;
        _functions["CFLT"] = BuiltinCflt;
        _functions["LTOC"] = BuiltinLtoc;
        _functions["CTOL"] = BuiltinCtol;
        _functions["TTOC"] = BuiltinTtoc;
        _functions["CTOT"] = BuiltinCtot;
        _functions["TTOF"] = BuiltinTtof;
        _functions["TTOR"] = BuiltinTtor;
        _functions["RTOT"] = BuiltinRtot;
        // Register function modules (pass FieldManager for state access)
        DateTimeFunctions.Register(_functions);
        MathFunctions.Register(_functions);
        StringFunctions.Register(_functions);
        SystemFunctions.Register(_functions, _fields);
        FileFunctions.Register(_functions, _fields);
        ScreenFunctions.Register(_functions, _fields);
        FieldInfoFunctions.Register(_functions, _fields);
    }

    public TasValue Evaluate(Expression expr)
    {
        return expr switch
        {
            LiteralExpr lit => EvaluateLiteral(lit),
            IdentifierExpr id => _fields.Get(id.Name),
            ArrayAccessExpr arr => _fields.GetArrayElement(arr.Name, (int)Evaluate(arr.Index).AsNumeric()),
            BinaryExpr bin => EvaluateBinary(bin),
            UnaryExpr un => EvaluateUnary(un),
            FunctionCallExpr fn => EvaluateFunction(fn),
            _ => throw new InterpreterException($"Unknown expression type: {expr.GetType().Name}"),
        };
    }

    private static TasValue EvaluateLiteral(LiteralExpr lit)
    {
        return lit.Value switch
        {
            string s => new TasValue(TasType.Alpha, s, s.Length),
            int i => new TasValue(TasType.Integer, i, 10),
            double d => new TasValue(TasType.Numeric, d, 10, 2),
            bool b => new TasValue(TasType.Logical, b, 1),
            _ => throw new InterpreterException($"Unknown literal type: {lit.Value?.GetType().Name}"),
        };
    }

    private TasValue EvaluateBinary(BinaryExpr bin)
    {
        var left = Evaluate(bin.Left);
        var right = Evaluate(bin.Right);

        return bin.Operator switch
        {
            "+" when left.Type == TasType.Alpha || right.Type == TasType.Alpha =>
                new TasValue(TasType.Alpha, left.AsString() + right.AsString()),
            "+" => NumericOp(left, right, (a, b) => a + b),
            "-" => NumericOp(left, right, (a, b) => a - b),
            "*" => NumericOp(left, right, (a, b) => a * b),
            "/" => NumericOp(left, right, (a, b) => b != 0 ? a / b : 0),

            "=" => Compare(left, right, (c) => c == 0),
            "<>" => Compare(left, right, (c) => c != 0),
            "<" => Compare(left, right, (c) => c < 0),
            "<=" => Compare(left, right, (c) => c <= 0),
            ">" => Compare(left, right, (c) => c > 0),
            ">=" => Compare(left, right, (c) => c >= 0),

            ".AND." => new TasValue(TasType.Logical, left.AsLogical() && right.AsLogical()),
            ".OR." => new TasValue(TasType.Logical, left.AsLogical() || right.AsLogical()),

            _ => throw new InterpreterException($"Unknown operator: {bin.Operator}"),
        };
    }

    private TasValue EvaluateUnary(UnaryExpr un)
    {
        var operand = Evaluate(un.Operand);
        return un.Operator switch
        {
            "-" => new TasValue(TasType.Numeric, -operand.AsNumeric()),
            ".NOT." => new TasValue(TasType.Logical, !operand.AsLogical()),
            _ => throw new InterpreterException($"Unknown unary operator: {un.Operator}"),
        };
    }

    private TasValue EvaluateFunction(FunctionCallExpr fn)
    {
        // Check if it's actually an array access
        if (_fields.IsArray(fn.Name) && fn.Arguments.Count == 1)
        {
            int index = (int)Evaluate(fn.Arguments[0]).AsNumeric();
            return _fields.GetArrayElement(fn.Name, index);
        }

        var args = fn.Arguments.Select(Evaluate).ToList();
        string name = fn.Name.ToUpperInvariant();

        if (_functions.TryGetValue(name, out var func))
            return func(args);

        // Try UDF callback (user-defined functions)
        if (UdfCallback != null)
        {
            var result = UdfCallback(fn.Name, args);
            if (result != null)
                return result;
        }

        throw new InterpreterException($"Unknown function: {fn.Name}");
    }

    private static TasValue NumericOp(TasValue left, TasValue right, Func<double, double, double> op)
    {
        double result = op(left.AsNumeric(), right.AsNumeric());
        if (left.Type == TasType.Integer && right.Type == TasType.Integer && result == (int)result)
            return new TasValue(TasType.Integer, (int)result);
        return new TasValue(TasType.Numeric, result);
    }

    private static TasValue Compare(TasValue left, TasValue right, Func<int, bool> predicate)
    {
        int cmp;
        if (left.Type == TasType.Alpha || right.Type == TasType.Alpha)
            cmp = string.Compare(left.AsString().TrimEnd(), right.AsString().TrimEnd(), StringComparison.OrdinalIgnoreCase);
        else
            cmp = left.AsNumeric().CompareTo(right.AsNumeric());

        return new TasValue(TasType.Logical, predicate(cmp));
    }

    // --- Built-in functions (kept inline for the original 11 + conversion functions) ---

    private static TasValue BuiltinTrim(List<TasValue> args)
    {
        ExpectArgRange("TRIM", args, 1, 2);
        string s = args[0].AsString();
        string side = args.Count > 1 ? args[1].AsString().ToUpperInvariant().Trim() : "B";
        s = side switch
        {
            "L" => s.TrimStart(),
            "R" => s.TrimEnd(),
            _ => s.Trim(), // "B" or default = both
        };
        return new TasValue(TasType.Alpha, s, s.Length);
    }

    private static TasValue BuiltinUp(List<TasValue> args)
    {
        ExpectArgCount("UP", args, 1);
        string s = args[0].AsString().ToUpperInvariant();
        return new TasValue(TasType.Alpha, s, s.Length);
    }

    private static TasValue BuiltinLow(List<TasValue> args)
    {
        ExpectArgCount("LOW", args, 1);
        string s = args[0].AsString().ToLowerInvariant();
        return new TasValue(TasType.Alpha, s, s.Length);
    }

    private static TasValue BuiltinStr(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("STR() requires at least 1 argument");
        string s = args[0].AsString();
        return new TasValue(TasType.Alpha, s, s.Length);
    }

    private static TasValue BuiltinVal(List<TasValue> args)
    {
        ExpectArgCount("VAL", args, 1);
        double v = args[0].AsNumeric();
        return new TasValue(TasType.Numeric, v);
    }

    private static TasValue BuiltinMid(List<TasValue> args)
    {
        if (args.Count < 2) throw new InterpreterException("MID() requires at least 2 arguments");
        string s = args[0].AsString();
        int start = args[1].AsInteger() - 1; // TAS is 1-based
        int len = args.Count >= 3 ? args[2].AsInteger() : s.Length - start;
        if (start < 0) start = 0;
        if (start >= s.Length) return new TasValue(TasType.Alpha, "", 0);
        if (start + len > s.Length) len = s.Length - start;
        string result = s.Substring(start, len);
        return new TasValue(TasType.Alpha, result, result.Length);
    }

    private static TasValue BuiltinLen(List<TasValue> args)
    {
        ExpectArgCount("LEN", args, 1);
        return new TasValue(TasType.Integer, args[0].AsString().TrimEnd().Length);
    }

    private static TasValue BuiltinChr(List<TasValue> args)
    {
        ExpectArgCount("CHR", args, 1);
        char c = (char)args[0].AsInteger();
        return new TasValue(TasType.Alpha, c.ToString(), 1);
    }

    private static TasValue BuiltinAsc(List<TasValue> args)
    {
        ExpectArgCount("ASC", args, 1);
        string s = args[0].AsString();
        return new TasValue(TasType.Integer, s.Length > 0 ? (int)s[0] : 0);
    }

    private static TasValue BuiltinAbs(List<TasValue> args)
    {
        ExpectArgCount("ABS", args, 1);
        return new TasValue(TasType.Numeric, Math.Abs(args[0].AsNumeric()));
    }

    private static TasValue BuiltinInt(List<TasValue> args)
    {
        ExpectArgCount("INT", args, 1);
        return new TasValue(TasType.Integer, (int)args[0].AsNumeric());
    }

    private static TasValue BuiltinClnum(List<TasValue> args)
    {
        // CLNUM(string) - strip non-numeric characters
        ExpectArgCount("CLNUM", args, 1);
        string s = args[0].AsString();
        string result = new string(s.Where(c => char.IsDigit(c) || c == '.' || c == '-').ToArray());
        return new TasValue(TasType.Alpha, result, result.Length);
    }

    private static TasValue BuiltinCint(List<TasValue> args)
    {
        // CINT(value) - convert to integer
        ExpectArgCount("CINT", args, 1);
        return new TasValue(TasType.Integer, args[0].AsInteger());
    }

    private static TasValue BuiltinCflt(List<TasValue> args)
    {
        // CFLT(value) - convert to float/numeric
        ExpectArgCount("CFLT", args, 1);
        return new TasValue(TasType.Numeric, args[0].AsNumeric());
    }

    private static TasValue BuiltinLtoc(List<TasValue> args)
    {
        // LTOC(logical) - logical to character (.T./.F.)
        ExpectArgCount("LTOC", args, 1);
        string s = args[0].AsLogical() ? ".T." : ".F.";
        return new TasValue(TasType.Alpha, s, 3);
    }

    private static TasValue BuiltinCtol(List<TasValue> args)
    {
        // CTOL(string) - character to logical
        ExpectArgCount("CTOL", args, 1);
        return new TasValue(TasType.Logical, args[0].AsLogical());
    }

    private static TasValue BuiltinTtoc(List<TasValue> args)
    {
        // TTOC(time) - time to character
        if (args.Count < 1) return new TasValue(TasType.Alpha, "", 8);
        return new TasValue(TasType.Alpha, args[0].AsString(), 8);
    }

    private static TasValue BuiltinCtot(List<TasValue> args)
    {
        // CTOT(string) - character to time
        if (args.Count < 1) return new TasValue(TasType.Time, "", 6);
        return new TasValue(TasType.Time, args[0].AsString().Trim(), 6);
    }

    private static TasValue BuiltinTtof(List<TasValue> args)
    {
        // TTOF(time) - time to float (seconds since midnight)
        if (args.Count < 1) return new TasValue(TasType.Numeric, 0.0);
        string s = args[0].AsString().Trim();
        if (TimeSpan.TryParse(s, out var ts))
            return new TasValue(TasType.Numeric, ts.TotalSeconds);
        return new TasValue(TasType.Numeric, 0.0);
    }

    private static TasValue BuiltinTtor(List<TasValue> args)
    {
        // TTOR(time) - time to real (same as TTOF)
        return BuiltinTtof(args);
    }

    private static TasValue BuiltinRtot(List<TasValue> args)
    {
        // RTOT(seconds) - real to time string
        if (args.Count < 1) return new TasValue(TasType.Alpha, "00:00:00", 8);
        double seconds = args[0].AsNumeric();
        var ts = TimeSpan.FromSeconds(seconds);
        string s = ts.ToString(@"hh\:mm\:ss");
        return new TasValue(TasType.Alpha, s, 8);
    }

    private static void ExpectArgCount(string name, List<TasValue> args, int expected)
    {
        if (args.Count != expected)
            throw new InterpreterException($"{name}() expects {expected} argument(s), got {args.Count}");
    }

    private static void ExpectArgRange(string name, List<TasValue> args, int min, int max)
    {
        if (args.Count < min || args.Count > max)
            throw new InterpreterException($"{name}() expects {min}-{max} argument(s), got {args.Count}");
    }
}
