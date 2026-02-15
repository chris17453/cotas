namespace CoTAS.Parser.Ast;

public sealed record LiteralExpr(object Value, int Line) : Expression(Line);

public sealed record IdentifierExpr(string Name, int Line) : Expression(Line);

public sealed record ArrayAccessExpr(string Name, Expression Index, int Line) : Expression(Line);

public sealed record BinaryExpr(Expression Left, string Operator, Expression Right, int Line) : Expression(Line);

public sealed record UnaryExpr(string Operator, Expression Operand, int Line) : Expression(Line);

public sealed record FunctionCallExpr(string Name, List<Expression> Arguments, int Line) : Expression(Line);
