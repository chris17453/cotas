namespace CoTAS.Interpreter;

public enum TasType
{
    Alpha,
    Numeric,
    Integer,
    Date,
    Time,
    Logical,
}

public sealed class TasValue
{
    public TasType Type { get; }
    public object Value { get; set; }
    public int Size { get; }
    public int Decimals { get; }

    public TasValue(TasType type, object value, int size = 0, int decimals = 0)
    {
        Type = type;
        Value = value;
        Size = size;
        Decimals = decimals;
    }

    public static TasValue DefaultForType(string? typeCode, int size = 10, int decimals = 0)
    {
        return (typeCode?.ToUpperInvariant()) switch
        {
            "A" => new TasValue(TasType.Alpha, new string(' ', size), size),
            "N" => new TasValue(TasType.Numeric, 0.0, size, decimals),
            "I" => new TasValue(TasType.Integer, 0, size),
            "D" => new TasValue(TasType.Date, "", 8),
            "T" => new TasValue(TasType.Time, "", 6),
            "L" => new TasValue(TasType.Logical, false, 1),
            _ => new TasValue(TasType.Alpha, new string(' ', size), size),
        };
    }

    public string AsString()
    {
        return Value switch
        {
            string s => s,
            int i => i.ToString(),
            double d => Decimals > 0 ? d.ToString($"F{Decimals}") : d.ToString("G"),
            bool b => b ? ".T." : ".F.",
            _ => Value?.ToString() ?? "",
        };
    }

    public double AsNumeric()
    {
        return Value switch
        {
            double d => d,
            int i => i,
            string s => double.TryParse(s.Trim(), out var r) ? r : 0.0,
            bool b => b ? 1.0 : 0.0,
            _ => 0.0,
        };
    }

    public int AsInteger()
    {
        return Value switch
        {
            int i => i,
            double d => (int)d,
            string s => int.TryParse(s.Trim(), out var r) ? r : 0,
            bool b => b ? 1 : 0,
            _ => 0,
        };
    }

    public bool AsLogical()
    {
        return Value switch
        {
            bool b => b,
            int i => i != 0,
            double d => d != 0.0,
            string s => !string.IsNullOrWhiteSpace(s) && s.Trim().ToUpper() != ".F." && s.Trim() != "0",
            _ => false,
        };
    }

    public TasValue Clone() => new(Type, Value, Size, Decimals);

    public override string ToString() => $"TasValue({Type}, {AsString()})";
}
