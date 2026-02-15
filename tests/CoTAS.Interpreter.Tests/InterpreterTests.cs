using CoTAS.Parser;
using CoTAS.Parser.Ast;
using CoTAS.Interpreter;
using CoTAS.Bridge;

namespace CoTAS.Interpreter.Tests;

public class TestBridge : IUIBridge
{
    public List<(string Text, int Row, int Col)> SayOutput { get; } = [];
    public List<string> Messages { get; } = [];
    public int ClearCount { get; private set; }

    public Task ClearScreenAsync() { ClearCount++; return Task.CompletedTask; }
    public Task SayAsync(string text, int row, int col) { SayOutput.Add((text, row, col)); return Task.CompletedTask; }
    public Task MessageAsync(string text) { Messages.Add(text); return Task.CompletedTask; }
    public Task<string> EnterAsync(string fieldName, int row, int col, int size) => Task.FromResult("");
    public Task<bool> AskAsync(string prompt, bool defaultValue) => Task.FromResult(defaultValue);
}

public class InterpreterTests
{
    private static async Task<(TestBridge Bridge, TasInterpreter Interpreter)> RunWithInterpreterAsync(string source)
    {
        var lexer = new Lexer(source);
        var tokens = lexer.Tokenize();
        var parser = new TasParser(tokens);
        var program = parser.ParseProgram();
        var bridge = new TestBridge();
        var interpreter = new TasInterpreter(bridge);
        await interpreter.ExecuteAsync(program);
        return (bridge, interpreter);
    }

    private static async Task<TestBridge> RunAsync(string source)
    {
        var (bridge, _) = await RunWithInterpreterAsync(source);
        return bridge;
    }

    [Fact]
    public async Task Execute_SayString()
    {
        var bridge = await RunAsync("say 'Hello' at 1,1\nquit");
        Assert.Single(bridge.SayOutput);
        Assert.Equal("Hello", bridge.SayOutput[0].Text);
        Assert.Equal(1, bridge.SayOutput[0].Row);
        Assert.Equal(1, bridge.SayOutput[0].Col);
    }

    [Fact]
    public async Task Execute_DefineAndAssign()
    {
        var bridge = await RunAsync("define x type I size 5\nx = 42\nsay str(x) at 1,1\nquit");
        Assert.Equal("42", bridge.SayOutput[0].Text);
    }

    [Fact]
    public async Task Execute_WhileLoop()
    {
        var bridge = await RunAsync(
"define i type I size 5\n" +
"i = 0\n" +
"while i < 3\n" +
"i = i + 1\n" +
"endw\n" +
"say str(i) at 1,1\n" +
"quit");
        Assert.Single(bridge.SayOutput);
        Assert.Equal("3", bridge.SayOutput[0].Text);
    }

    [Fact]
    public async Task Execute_GosubReturn()
    {
        var bridge = await RunAsync("""
            gosub MYSUB
            say 'after' at 1,2
            quit
            MYSUB:
                say 'in sub' at 1,1
                ret
            """);
        Assert.Equal(2, bridge.SayOutput.Count);
        Assert.Equal("in sub", bridge.SayOutput[0].Text);
        Assert.Equal("after", bridge.SayOutput[1].Text);
    }

    [Fact]
    public async Task Execute_IfThen()
    {
        var bridge = await RunAsync("""
            define x type I size 5
            x = 1
            if x = 1 then say 'yes' at 1,1
            if x = 2 then say 'no' at 1,2
            quit
            """);
        Assert.Single(bridge.SayOutput);
        Assert.Equal("yes", bridge.SayOutput[0].Text);
    }

    [Fact]
    public async Task Execute_ClearScreen()
    {
        var bridge = await RunAsync("clrscr\nquit");
        Assert.Equal(1, bridge.ClearCount);
    }

    [Fact]
    public async Task Execute_Message()
    {
        var bridge = await RunAsync("msg 'Done!'\nquit");
        Assert.Single(bridge.Messages);
        Assert.Equal("Done!", bridge.Messages[0]);
    }

    [Fact]
    public async Task Execute_StringConcatenation()
    {
        var bridge = await RunAsync("""
            define greeting type A size 30
            greeting = 'Hello' + ' ' + 'World'
            say greeting at 1,1
            quit
            """);
        Assert.Equal("Hello World", bridge.SayOutput[0].Text);
    }

    [Fact]
    public async Task Execute_ArithmeticExpressions()
    {
        var bridge = await RunAsync("""
            define x type I size 5
            x = 2 + 3 * 4
            say str(x) at 1,1
            quit
            """);
        Assert.Equal("14", bridge.SayOutput[0].Text);
    }

    // --- Phase 2A: New Tests ---

    [Fact]
    public async Task Execute_ForLoop()
    {
        var bridge = await RunAsync("""
            define total type N size 10
            total = 0
            for(i;1;5;1)
                total = total + i
            next
            say str(total) at 1,1
            quit
            """);
        // 1+2+3+4+5 = 15
        Assert.Equal("15", bridge.SayOutput[0].Text);
    }

    [Fact]
    public async Task Execute_ForLoopWithStep()
    {
        var bridge = await RunAsync("""
            define total type N size 10
            total = 0
            for(i;0;10;2)
                total = total + i
            next
            say str(total) at 1,1
            quit
            """);
        // 0+2+4+6+8+10 = 30
        Assert.Equal("30", bridge.SayOutput[0].Text);
    }

    [Fact]
    public async Task Execute_SelectCase()
    {
        var bridge = await RunAsync("""
            define color type A size 10
            color = 'blue'
            select color
            case 'red'
                say 'R' at 1,1
            case 'blue'
                say 'B' at 1,1
            case 'green'
                say 'G' at 1,1
            otherwise
                say '?' at 1,1
            endc
            quit
            """);
        Assert.Single(bridge.SayOutput);
        Assert.Equal("B", bridge.SayOutput[0].Text);
    }

    [Fact]
    public async Task Execute_SelectOtherwise()
    {
        var bridge = await RunAsync("""
            define x type I size 5
            x = 99
            select x
            case 1
                say 'one' at 1,1
            case 2
                say 'two' at 1,1
            otherwise
                say 'other' at 1,1
            endc
            quit
            """);
        Assert.Single(bridge.SayOutput);
        Assert.Equal("other", bridge.SayOutput[0].Text);
    }

    [Fact]
    public async Task Execute_MathFunctions_Round()
    {
        var bridge = await RunAsync("""
            say str(round(3.456, 2)) at 1,1
            say str(round(3.456, 0)) at 2,1
            quit
            """);
        Assert.Equal("3.46", bridge.SayOutput[0].Text);
        Assert.Equal("3", bridge.SayOutput[1].Text);
    }

    [Fact]
    public async Task Execute_MathFunctions_SqrtAndAbs()
    {
        var bridge = await RunAsync("""
            say str(sqrt(16)) at 1,1
            say str(abs(-42)) at 2,1
            quit
            """);
        Assert.Equal("4", bridge.SayOutput[0].Text);
        Assert.Equal("42", bridge.SayOutput[1].Text);
    }

    [Fact]
    public async Task Execute_StringFunctions_Loc()
    {
        var bridge = await RunAsync("""
            define pos type I size 5
            pos = loc('Hello World', 'World')
            say str(pos) at 1,1
            quit
            """);
        Assert.Equal("7", bridge.SayOutput[0].Text);
    }

    [Fact]
    public async Task Execute_StringFunctions_Seg()
    {
        var bridge = await RunAsync("""
            define result type A size 20
            result = seg('Hello World', 7, 5)
            say result at 1,1
            quit
            """);
        Assert.Equal("World", bridge.SayOutput[0].Text);
    }

    [Fact]
    public async Task Execute_StringFunctions_Null()
    {
        var bridge = await RunAsync("""
            define blank type A size 10
            if null(blank) then say 'empty' at 1,1
            blank = 'hello'
            if .not. null(blank) then say 'filled' at 2,1
            quit
            """);
        Assert.Equal(2, bridge.SayOutput.Count);
        Assert.Equal("empty", bridge.SayOutput[0].Text);
        Assert.Equal("filled", bridge.SayOutput[1].Text);
    }

    [Fact]
    public async Task Execute_IIF()
    {
        var bridge = await RunAsync("""
            define x type I size 5
            x = 10
            say iif(x > 5, 'big', 'small') at 1,1
            quit
            """);
        Assert.Equal("big", bridge.SayOutput[0].Text);
    }

    [Fact]
    public async Task Execute_DateFunction()
    {
        var bridge = await RunAsync("""
            define d type A size 10
            d = date()
            say d at 1,1
            quit
            """);
        Assert.Single(bridge.SayOutput);
        // Just verify it returned something in MM/DD/YY format
        Assert.Matches(@"\d{2}/\d{2}/\d{2}", bridge.SayOutput[0].Text);
    }

    [Fact]
    public async Task Execute_IncDec()
    {
        var (bridge, interp) = await RunWithInterpreterAsync("""
            define counter type I size 5
            counter = 10
            inc counter
            inc counter
            dec counter
            say str(counter) at 1,1
            quit
            """);
        Assert.Equal("11", bridge.SayOutput[0].Text);
    }

    [Fact]
    public async Task Execute_WhileWithExit()
    {
        var bridge = await RunAsync("""
            define i type I size 5
            i = 0
            while .t.
                i = i + 1
                if i = 3 then exit
            endw
            say str(i) at 1,1
            quit
            """);
        Assert.Equal("3", bridge.SayOutput[0].Text);
    }

    [Fact]
    public async Task Execute_GenericCommand_Stub()
    {
        // Verify stub commands don't crash
        var bridge = await RunAsync("""
            bell
            trace
            norstrt
            say 'ok' at 1,1
            quit
            """);
        // BELL now outputs "\a" via SayAsync, so we have 2 outputs
        Assert.Equal(2, bridge.SayOutput.Count);
        Assert.Equal("\a", bridge.SayOutput[0].Text);
        Assert.Equal("ok", bridge.SayOutput[1].Text);
    }

    [Fact]
    public async Task Execute_ConversionFunctions()
    {
        var bridge = await RunAsync("""
            say str(cint(3.7)) at 1,1
            say ltoc(.t.) at 2,1
            quit
            """);
        Assert.Equal("3", bridge.SayOutput[0].Text);
        Assert.Equal(".T.", bridge.SayOutput[1].Text);
    }

    [Fact]
    public async Task Execute_IsNumIsAl()
    {
        var bridge = await RunAsync("""
            if isnum('123') then say 'num' at 1,1
            if isal('abc') then say 'alpha' at 2,1
            if .not. isnum('abc') then say 'not num' at 3,1
            quit
            """);
        Assert.Equal(3, bridge.SayOutput.Count);
        Assert.Equal("num", bridge.SayOutput[0].Text);
        Assert.Equal("alpha", bridge.SayOutput[1].Text);
        Assert.Equal("not num", bridge.SayOutput[2].Text);
    }
}
