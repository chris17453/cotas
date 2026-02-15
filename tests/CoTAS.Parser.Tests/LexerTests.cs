using CoTAS.Parser;

namespace CoTAS.Parser.Tests;

public class LexerTests
{
    [Fact]
    public void Tokenize_SimpleAssignment()
    {
        var lexer = new Lexer("x = 42");
        var tokens = lexer.Tokenize();

        Assert.Equal(TokenType.Identifier, tokens[0].Type);
        Assert.Equal("x", tokens[0].Value);
        Assert.Equal(TokenType.Equal, tokens[1].Type);
        Assert.Equal(TokenType.IntegerLiteral, tokens[2].Type);
        Assert.Equal("42", tokens[2].Value);
    }

    [Fact]
    public void Tokenize_StringLiterals()
    {
        var lexer = new Lexer("say 'hello' at 1,1");
        var tokens = lexer.Tokenize();

        Assert.Equal(TokenType.Say, tokens[0].Type);
        Assert.Equal(TokenType.StringLiteral, tokens[1].Type);
        Assert.Equal("hello", tokens[1].Value);
        Assert.Equal(TokenType.At, tokens[2].Type);
    }

    [Fact]
    public void Tokenize_LogicalConstants()
    {
        var lexer = new Lexer(".T. .F. .A. .O. .N.");
        var tokens = lexer.Tokenize();

        Assert.Equal(TokenType.True, tokens[0].Type);
        Assert.Equal(TokenType.False, tokens[1].Type);
        Assert.Equal(TokenType.And, tokens[2].Type);
        Assert.Equal(TokenType.Or, tokens[3].Type);
        Assert.Equal(TokenType.Not, tokens[4].Type);
    }

    [Fact]
    public void Tokenize_DefineStatement()
    {
        var lexer = new Lexer("define name type A size 30");
        var tokens = lexer.Tokenize();

        Assert.Equal(TokenType.Define, tokens[0].Type);
        Assert.Equal(TokenType.Identifier, tokens[1].Type);
        Assert.Equal("name", tokens[1].Value);
        Assert.Equal(TokenType.Type, tokens[2].Type);
        Assert.Equal(TokenType.Identifier, tokens[3].Type);
        Assert.Equal("A", tokens[3].Value);
        Assert.Equal(TokenType.Size, tokens[4].Type);
        Assert.Equal(TokenType.IntegerLiteral, tokens[5].Type);
    }

    [Fact]
    public void Tokenize_Comments()
    {
        var lexer = new Lexer("; this is a comment\nx = 1");
        var tokens = lexer.Tokenize();

        Assert.Equal(TokenType.Comment, tokens[0].Type);
        Assert.Equal(TokenType.Newline, tokens[1].Type);
        Assert.Equal(TokenType.Identifier, tokens[2].Type);
    }

    [Fact]
    public void Tokenize_Preprocessor()
    {
        var lexer = new Lexer("#udx\ndefine x type I size 5");
        var tokens = lexer.Tokenize();

        Assert.Equal(TokenType.Preprocessor, tokens[0].Type);
        Assert.Equal("#udx", tokens[0].Value);
    }

    [Fact]
    public void Tokenize_Label()
    {
        var lexer = new Lexer("MAIN_LOOP:\n  x = 1");
        var tokens = lexer.Tokenize();

        Assert.Equal(TokenType.Label, tokens[0].Type);
        Assert.Equal("MAIN_LOOP", tokens[0].Value);
    }

    [Fact]
    public void Tokenize_ComparisonOperators()
    {
        var lexer = new Lexer("<> <= >= < > =");
        var tokens = lexer.Tokenize();

        Assert.Equal(TokenType.NotEqual, tokens[0].Type);
        Assert.Equal(TokenType.LessEqual, tokens[1].Type);
        Assert.Equal(TokenType.GreaterEqual, tokens[2].Type);
        Assert.Equal(TokenType.LessThan, tokens[3].Type);
        Assert.Equal(TokenType.GreaterThan, tokens[4].Type);
        Assert.Equal(TokenType.Equal, tokens[5].Type);
    }

    [Fact]
    public void Tokenize_NumericLiteral()
    {
        var lexer = new Lexer("123.45");
        var tokens = lexer.Tokenize();

        Assert.Equal(TokenType.NumericLiteral, tokens[0].Type);
        Assert.Equal("123.45", tokens[0].Value);
    }
}
