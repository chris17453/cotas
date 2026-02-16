namespace CoTAS.Parser.Ast;

public sealed record DefineStmt(
    List<string> FieldNames,
    string? FieldType,
    int? Size,
    int? Decimals,
    int? ArraySize,
    bool Reset,
    int Line) : Statement(Line);

public sealed record AssignmentStmt(string Target, Expression? Index, Expression Value, int Line) : Statement(Line);

public sealed record SayStmt(Expression Text, Expression? Row, Expression? Col, int Line) : Statement(Line);

public sealed record LabelStmt(string Name, int Line) : Statement(Line);

public sealed record GotoStmt(string Label, int Line) : Statement(Line);

public sealed record GosubStmt(string Label, int Line) : Statement(Line);

public sealed record ReturnStmt(Expression? ReturnValue, int Line) : Statement(Line);

public sealed record IfThenStmt(Expression Condition, Statement ThenBranch, int Line) : Statement(Line);

public sealed record IfBlockStmt(Expression Condition, List<Statement> ThenBlock, List<Statement>? ElseBlock, int Line) : Statement(Line);

public sealed record WhileStmt(Expression Condition, List<Statement> Body, int Line) : Statement(Line);

public sealed record QuitStmt(int Line) : Statement(Line);

public sealed record ClearScreenStmt(int Line) : Statement(Line);

public sealed record MessageStmt(Expression Text, int Line, bool NoWait = false, Expression? WindowsParam = null) : Statement(Line);

public sealed record PreprocessorStmt(string Text, int Line) : Statement(Line);

public sealed record ExpressionStmt(Expression Expr, int Line) : Statement(Line);

// Structured programming blocks
public sealed record SelectStmt(Expression Selector, List<(Expression? CaseValue, List<Statement> Body)> Cases, int Line) : Statement(Line);

public sealed record ForStmt(string Counter, Expression Start, Expression Stop, Expression Step, List<Statement> Body, int Line) : Statement(Line);

public sealed record ScanStmt(List<Token> Options, List<Statement> Body, int Line) : Statement(Line);

// Generic command: any TAS command we don't specifically handle yet.
// Captures the command name and all tokens until end of line.
public sealed record GenericCommandStmt(string CommandName, List<Token> Tokens, int Line) : Statement(Line);

// Loop control
public sealed record ExitStmt(int Line) : Statement(Line);
public sealed record LoopStmt(int Line) : Statement(Line);
