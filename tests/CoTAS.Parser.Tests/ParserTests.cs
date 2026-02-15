using CoTAS.Parser;
using CoTAS.Parser.Ast;

namespace CoTAS.Parser.Tests;

public class ParserTests
{
    private static TasProgram Parse(string source)
    {
        var lexer = new Lexer(source);
        var tokens = lexer.Tokenize();
        var parser = new TasParser(tokens);
        return parser.ParseProgram();
    }

    [Fact]
    public void Parse_DefineStatement()
    {
        var program = Parse("define name type A size 30");
        Assert.Single(program.Statements);
        var def = Assert.IsType<DefineStmt>(program.Statements[0]);
        Assert.Equal("name", def.FieldNames[0]);
        Assert.Equal("A", def.FieldType);
        Assert.Equal(30, def.Size);
    }

    [Fact]
    public void Parse_Assignment()
    {
        var program = Parse("x = 42");
        Assert.Single(program.Statements);
        var assign = Assert.IsType<AssignmentStmt>(program.Statements[0]);
        Assert.Equal("x", assign.Target);
        var lit = Assert.IsType<LiteralExpr>(assign.Value);
        Assert.Equal(42, lit.Value);
    }

    [Fact]
    public void Parse_SayWithAt()
    {
        var program = Parse("say 'hello' at 1,3");
        Assert.Single(program.Statements);
        var say = Assert.IsType<SayStmt>(program.Statements[0]);
        var text = Assert.IsType<LiteralExpr>(say.Text);
        Assert.Equal("hello", text.Value);
        Assert.NotNull(say.Row);
        Assert.NotNull(say.Col);
    }

    [Fact]
    public void Parse_IfThenSingleLine()
    {
        var program = Parse("if x = 1 then say 'yes' at 1,1");
        Assert.Single(program.Statements);
        var ifStmt = Assert.IsType<IfThenStmt>(program.Statements[0]);
        Assert.IsType<BinaryExpr>(ifStmt.Condition);
        Assert.IsType<SayStmt>(ifStmt.ThenBranch);
    }

    [Fact]
    public void Parse_WhileBlock()
    {
        var program = Parse("while x < 5\n  x = x + 1\nendw");
        Assert.Single(program.Statements);
        var w = Assert.IsType<WhileStmt>(program.Statements[0]);
        Assert.Single(w.Body);
    }

    [Fact]
    public void Parse_GosubAndReturn()
    {
        var program = Parse("gosub TEST\nquit\nTEST:\nret");
        Assert.Equal(4, program.Statements.Count);
        Assert.IsType<GosubStmt>(program.Statements[0]);
        Assert.IsType<QuitStmt>(program.Statements[1]);
        Assert.IsType<LabelStmt>(program.Statements[2]);
        Assert.IsType<ReturnStmt>(program.Statements[3]);
    }

    [Fact]
    public void Parse_BinaryExpression()
    {
        var program = Parse("x = 1 + 2 * 3");
        var assign = Assert.IsType<AssignmentStmt>(program.Statements[0]);
        // Should parse as 1 + (2 * 3) due to precedence
        var add = Assert.IsType<BinaryExpr>(assign.Value);
        Assert.Equal("+", add.Operator);
        var mul = Assert.IsType<BinaryExpr>(add.Right);
        Assert.Equal("*", mul.Operator);
    }

    [Fact]
    public void Parse_FunctionCall()
    {
        var program = Parse("x = trim(name)");
        var assign = Assert.IsType<AssignmentStmt>(program.Statements[0]);
        var fn = Assert.IsType<FunctionCallExpr>(assign.Value);
        Assert.Equal("trim", fn.Name);
        Assert.Single(fn.Arguments);
    }
}
