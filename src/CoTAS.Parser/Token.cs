namespace CoTAS.Parser;

public sealed record Token(
    TokenType Type,
    string Value,
    int Line,
    int Column)
{
    public override string ToString() => $"{Type}({Value}) at {Line}:{Column}";
}
