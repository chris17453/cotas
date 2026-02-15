namespace CoTAS.Parser.Ast;

public abstract record AstNode(int Line);

public abstract record Expression(int Line) : AstNode(Line);

public abstract record Statement(int Line) : AstNode(Line);
