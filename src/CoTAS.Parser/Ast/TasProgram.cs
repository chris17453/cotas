namespace CoTAS.Parser.Ast;

public sealed record TasProgram(List<Statement> Statements, int Line) : AstNode(Line);
