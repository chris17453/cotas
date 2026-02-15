namespace CoTAS.Interpreter.Functions;

/// <summary>
/// Math functions: ROUND, SQRT, MOD, SIGN, CEIL, FLOOR, RNDM, PI, SIN, COS, TAN, ASIN, ACOS, ATAN, EXP, LOG, LOG10
/// </summary>
public static class MathFunctions
{
    private static readonly Random _rng = new();

    public static void Register(Dictionary<string, Func<List<TasValue>, TasValue>> registry)
    {
        registry["ROUND"] = Round;
        registry["SQRT"] = Sqrt;
        registry["MOD"] = Mod;
        registry["SIGN"] = Sign;
        registry["CEIL"] = Ceil;
        registry["FLOOR"] = Floor;
        registry["RNDM"] = Rndm;
        registry["PI"] = Pi;
        registry["SIN"] = Sin;
        registry["COS"] = Cos;
        registry["TAN"] = Tan;
        registry["ASIN"] = Asin;
        registry["ACOS"] = Acos;
        registry["ATAN"] = Atan;
        registry["ATAN2"] = Atan2;
        registry["EXP"] = Exp;
        registry["LOG"] = Log;
        registry["LOG10"] = Log10;
        registry["DTOR"] = Dtor;
        registry["RTOD"] = Rtod;
    }

    private static TasValue Round(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("ROUND() requires at least 1 argument");
        double val = args[0].AsNumeric();
        int decimals = args.Count >= 2 ? args[1].AsInteger() : 0;
        return new TasValue(TasType.Numeric, Math.Round(val, decimals, MidpointRounding.AwayFromZero), 10, decimals);
    }

    private static TasValue Sqrt(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("SQRT() requires 1 argument");
        return new TasValue(TasType.Numeric, Math.Sqrt(args[0].AsNumeric()));
    }

    private static TasValue Mod(List<TasValue> args)
    {
        if (args.Count < 2) throw new InterpreterException("MOD() requires 2 arguments");
        double a = args[0].AsNumeric();
        double b = args[1].AsNumeric();
        return new TasValue(TasType.Numeric, b != 0 ? a % b : 0);
    }

    private static TasValue Sign(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("SIGN() requires 1 argument");
        return new TasValue(TasType.Integer, Math.Sign(args[0].AsNumeric()));
    }

    private static TasValue Ceil(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("CEIL() requires 1 argument");
        return new TasValue(TasType.Integer, (int)Math.Ceiling(args[0].AsNumeric()));
    }

    private static TasValue Floor(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("FLOOR() requires 1 argument");
        return new TasValue(TasType.Integer, (int)Math.Floor(args[0].AsNumeric()));
    }

    private static TasValue Rndm(List<TasValue> args)
    {
        // RNDM(max) returns random integer 0..max
        int max = args.Count >= 1 ? args[0].AsInteger() : 100;
        return new TasValue(TasType.Integer, _rng.Next(max + 1));
    }

    private static TasValue Pi(List<TasValue> args)
    {
        return new TasValue(TasType.Numeric, Math.PI);
    }

    private static TasValue Sin(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("SIN() requires 1 argument");
        return new TasValue(TasType.Numeric, Math.Sin(args[0].AsNumeric()));
    }

    private static TasValue Cos(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("COS() requires 1 argument");
        return new TasValue(TasType.Numeric, Math.Cos(args[0].AsNumeric()));
    }

    private static TasValue Tan(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("TAN() requires 1 argument");
        return new TasValue(TasType.Numeric, Math.Tan(args[0].AsNumeric()));
    }

    private static TasValue Asin(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("ASIN() requires 1 argument");
        return new TasValue(TasType.Numeric, Math.Asin(args[0].AsNumeric()));
    }

    private static TasValue Acos(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("ACOS() requires 1 argument");
        return new TasValue(TasType.Numeric, Math.Acos(args[0].AsNumeric()));
    }

    private static TasValue Atan(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("ATAN() requires 1 argument");
        return new TasValue(TasType.Numeric, Math.Atan(args[0].AsNumeric()));
    }

    private static TasValue Atan2(List<TasValue> args)
    {
        if (args.Count < 2) throw new InterpreterException("ATAN2() requires 2 arguments");
        return new TasValue(TasType.Numeric, Math.Atan2(args[0].AsNumeric(), args[1].AsNumeric()));
    }

    private static TasValue Exp(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("EXP() requires 1 argument");
        return new TasValue(TasType.Numeric, Math.Exp(args[0].AsNumeric()));
    }

    private static TasValue Log(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("LOG() requires 1 argument");
        return new TasValue(TasType.Numeric, Math.Log(args[0].AsNumeric()));
    }

    private static TasValue Log10(List<TasValue> args)
    {
        if (args.Count < 1) throw new InterpreterException("LOG10() requires 1 argument");
        return new TasValue(TasType.Numeric, Math.Log10(args[0].AsNumeric()));
    }

    private static TasValue Dtor(List<TasValue> args)
    {
        // Degrees to radians
        if (args.Count < 1) throw new InterpreterException("DTOR() requires 1 argument");
        return new TasValue(TasType.Numeric, args[0].AsNumeric() * Math.PI / 180.0);
    }

    private static TasValue Rtod(List<TasValue> args)
    {
        // Radians to degrees
        if (args.Count < 1) throw new InterpreterException("RTOD() requires 1 argument");
        return new TasValue(TasType.Numeric, args[0].AsNumeric() * 180.0 / Math.PI);
    }
}
