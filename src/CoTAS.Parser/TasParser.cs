using CoTAS.Parser.Ast;

namespace CoTAS.Parser;

public sealed class TasParser
{
    private readonly List<Token> _tokens;
    private int _pos;

    public TasParser(List<Token> tokens)
    {
        _tokens = tokens.Where(t => t.Type != TokenType.Comment).ToList();
    }

    /// <summary>
    /// Parse a single expression from the token stream. Used by the interpreter
    /// to parse SCAN WHILE/FOR clauses at runtime.
    /// </summary>
    public Expression ParseExpressionPublic() => ParseExpression();

    public TasProgram ParseProgram()
    {
        var statements = new List<Statement>();
        SkipNewlines();

        while (!IsAtEnd)
        {
            int before = _pos;
            try
            {
                var stmt = ParseStatement();
                if (stmt != null)
                    statements.Add(stmt);
            }
            catch (ParseException)
            {
                // Recovery: skip to next line
                SkipToEndOfLine();
            }
            SkipNewlines();
            if (_pos == before)
            {
                // No progress at top level - skip this token to avoid infinite loop
                Advance();
            }
        }

        return new TasProgram(statements, 1);
    }

    private Statement? ParseStatement()
    {
        var token = Current;

        return token.Type switch
        {
            TokenType.Preprocessor => ParsePreprocessor(),
            TokenType.Label => ParseLabel(),
            TokenType.Define => ParseDefine(),
            TokenType.If => ParseIf(),
            TokenType.While => ParseWhile(),
            TokenType.For => ParseFor(),
            TokenType.Select => ParseSelect(),
            TokenType.Scan => ParseScan(),
            TokenType.Goto => ParseGoto(),
            TokenType.Gosub => ParseGosub(),
            TokenType.Ret => ParseReturn(),
            TokenType.Say => ParseSay(),
            TokenType.Msg => ParseMessage(),
            TokenType.Quit => ParseQuit(),
            TokenType.ClrScr => ParseClearScreen(),
            TokenType.Exit or TokenType.ExitIf => ParseExit(),
            TokenType.Loop or TokenType.LoopIf => ParseLoop(),
            TokenType.Fexit or TokenType.FexitIf => ParseExit(),
            TokenType.Floop or TokenType.FloopIf => ParseLoop(),
            TokenType.Sexit or TokenType.SexitIf => ParseExit(),
            TokenType.Sloop or TokenType.SloopIf => ParseLoop(),
            // These keywords should never appear as a statement start (they're block terminators)
            TokenType.EndIf or TokenType.EndW or TokenType.EndC or TokenType.EndS
                or TokenType.Next or TokenType.Else or TokenType.ElseIf
                or TokenType.Case or TokenType.Otherwise => null, // handled by parent block
            TokenType.Identifier => ParseIdentifierStatement(),
            TokenType.Newline => SkipNewlinesAndReturnNull(),
            TokenType.Eof => null,
            // Any other token at statement start: treat as generic command
            _ => ParseGenericCommand(),
        };
    }

    private PreprocessorStmt ParsePreprocessor()
    {
        var token = Advance();
        return new PreprocessorStmt(token.Value, token.Line);
    }

    private LabelStmt ParseLabel()
    {
        var token = Advance();
        return new LabelStmt(token.Value, token.Line);
    }

    private DefineStmt ParseDefine()
    {
        int line = Current.Line;
        Advance(); // consume DEFINE

        var names = new List<string>();
        names.Add(ExpectAnyIdentifier("field name"));

        while (Check(TokenType.Comma))
        {
            Advance(); // comma
            names.Add(ExpectAnyIdentifier("field name"));
        }

        string? fieldType = null;
        int? size = null;
        int? decimals = null;
        int? arraySize = null;
        bool reset = false;

        while (!IsAtEndOfStatement)
        {
            if (Check(TokenType.Type))
            {
                Advance();
                fieldType = ExpectAnyIdentifier("type letter").ToUpperInvariant();
            }
            else if (Check(TokenType.Size))
            {
                Advance();
                size = ExpectInteger("size");
            }
            else if (Check(TokenType.Dec))
            {
                Advance();
                decimals = ExpectInteger("decimals");
            }
            else if (Check(TokenType.Array))
            {
                Advance();
                arraySize = ExpectInteger("array size");
            }
            else if (Check(TokenType.Reset))
            {
                Advance();
                reset = true;
            }
            else
            {
                break;
            }
        }

        return new DefineStmt(names, fieldType, size, decimals, arraySize, reset, line);
    }

    private Statement ParseIf()
    {
        int line = Current.Line;
        Advance(); // consume IF

        var condition = ParseExpression();

        // Single-line IF: if cond then stmt
        if (Check(TokenType.Then))
        {
            Advance(); // consume THEN
            var thenStmt = ParseStatement()!;
            return new IfThenStmt(condition, thenStmt, line);
        }

        // Single-line IF without THEN: if cond goto/gosub/ret/quit label
        if (Check(TokenType.Goto) || Check(TokenType.Gosub)
            || Check(TokenType.Ret) || Check(TokenType.Quit))
        {
            var thenStmt = ParseStatement()!;
            return new IfThenStmt(condition, thenStmt, line);
        }

        // Single-line IF: if cond <command-on-same-line> (no newline between condition and action)
        // Handles patterns like: if condition reent, if condition exit, etc.
        // But NOT "if cond DO" (DO starts a block) or "if cond" at end of line (block IF)
        if (!IsAtEndOfStatement
            && !(Check(TokenType.Identifier) && Current.Value.Equals("DO", StringComparison.OrdinalIgnoreCase)))
        {
            // There's still content on this line — treat as single-line IF
            var thenStmt = ParseStatement()!;
            return new IfThenStmt(condition, thenStmt, line);
        }

        // Block IF: if cond <newline> ... else/else_if ... endif
        // Skip DO keyword if present (TAS uses DO to start compound blocks)
        if (Check(TokenType.Identifier) && Current.Value.Equals("DO", StringComparison.OrdinalIgnoreCase))
            Advance();
        SkipNewlines();
        var thenBlock = new List<Statement>();
        List<Statement>? elseBlock = null;

        while (!IsAtEnd && !Check(TokenType.EndIf) && !Check(TokenType.Else) && !Check(TokenType.ElseIf))
        {
            if (!ParseBlockStatement(thenBlock)) break;
        }

        // Handle ELSE_IF chain by converting to nested IF
        if (Check(TokenType.ElseIf))
        {
            // Transform ELSE_IF into ELSE + nested IF
            var nestedIf = ParseIf(); // recurse — ELSE_IF acts like a new IF
            elseBlock = new List<Statement> { nestedIf };
        }
        else if (Check(TokenType.Else))
        {
            Advance();
            SkipNewlines();
            elseBlock = new List<Statement>();
            while (!IsAtEnd && !Check(TokenType.EndIf))
            {
                if (!ParseBlockStatement(elseBlock)) break;
            }
        }

        if (Check(TokenType.EndIf)) Advance();

        return new IfBlockStmt(condition, thenBlock, elseBlock, line);
    }

    private WhileStmt ParseWhile()
    {
        int line = Current.Line;
        Advance(); // consume WHILE
        var condition = ParseExpression();
        SkipNewlines();

        var body = new List<Statement>();
        while (!IsAtEnd && !Check(TokenType.EndW))
        {
            if (!ParseBlockStatement(body)) break;
        }

        if (Check(TokenType.EndW)) Advance();

        return new WhileStmt(condition, body, line);
    }

    private ForStmt ParseFor()
    {
        int line = Current.Line;
        Advance(); // consume FOR

        // FOR(counter;start;stop;step)
        Expect(TokenType.LeftParen, "'('");
        string counter = ExpectAnyIdentifier("counter");
        // Allow = after counter for variant syntax FOR(I=1;10;1)
        if (Check(TokenType.Equal)) Advance();
        // Separator can be ; or ,
        ExpectSeparator();
        var start = ParseExpression();
        ExpectSeparator();
        var stop = ParseExpression();
        ExpectSeparator();
        var step = ParseExpression();
        Expect(TokenType.RightParen, "')'");

        SkipNewlines();
        var body = new List<Statement>();
        while (!IsAtEnd && !Check(TokenType.Next))
        {
            if (!ParseBlockStatement(body)) break;
        }
        if (Check(TokenType.Next)) Advance();

        return new ForStmt(counter, start, stop, step, body, line);
    }

    private SelectStmt ParseSelect()
    {
        int line = Current.Line;
        Advance(); // consume SELECT
        var selector = ParseExpression();
        SkipNewlines();

        var cases = new List<(Expression? CaseValue, List<Statement> Body)>();

        while (!IsAtEnd && !Check(TokenType.EndC))
        {
            int outerBefore = _pos;
            if (Check(TokenType.Case))
            {
                Advance();
                var caseValue = ParseExpression();
                SkipNewlines();
                var caseBody = new List<Statement>();
                while (!IsAtEnd && !Check(TokenType.Case) && !Check(TokenType.Otherwise) && !Check(TokenType.EndC))
                {
                    if (!ParseBlockStatement(caseBody)) break;
                }
                cases.Add((caseValue, caseBody));
            }
            else if (Check(TokenType.Otherwise))
            {
                Advance();
                SkipNewlines();
                var otherwiseBody = new List<Statement>();
                while (!IsAtEnd && !Check(TokenType.EndC))
                {
                    if (!ParseBlockStatement(otherwiseBody)) break;
                }
                cases.Add((null, otherwiseBody));
            }
            else
            {
                SkipNewlines();
                // Skip any stray tokens between SELECT and first CASE
                if (!Check(TokenType.Case) && !Check(TokenType.Otherwise) && !Check(TokenType.EndC) && !IsAtEnd)
                {
                    Advance(); // Force progress to avoid infinite loop
                }
            }
            if (_pos == outerBefore) break;
        }

        if (Check(TokenType.EndC)) Advance();

        return new SelectStmt(selector, cases, line);
    }

    private ScanStmt ParseScan()
    {
        int line = Current.Line;
        Advance(); // consume SCAN

        // Collect all tokens until end of line as options
        var options = CollectRestOfLine();
        SkipNewlines();

        var body = new List<Statement>();
        while (!IsAtEnd && !Check(TokenType.EndS))
        {
            if (!ParseBlockStatement(body)) break;
        }
        if (Check(TokenType.EndS)) Advance();

        return new ScanStmt(options, body, line);
    }

    private GotoStmt ParseGoto()
    {
        int line = Current.Line;
        Advance();
        string label = ExpectAnyIdentifier("label");
        return new GotoStmt(label, line);
    }

    private GosubStmt ParseGosub()
    {
        int line = Current.Line;
        Advance();
        string label = ExpectAnyIdentifier("label");
        return new GosubStmt(label, line);
    }

    private ReturnStmt ParseReturn()
    {
        int line = Current.Line;
        Advance();
        // RET can optionally be followed by an expression (UDF return value)
        Expression? returnValue = null;
        if (!IsAtEndOfStatement)
        {
            try
            {
                returnValue = ParseExpression();
            }
            catch
            {
                // If expression parsing fails, just skip the rest of the line
                returnValue = null;
            }
        }
        SkipToEndOfLine();
        return new ReturnStmt(returnValue, line);
    }

    private SayStmt ParseSay()
    {
        int line = Current.Line;
        Advance(); // consume SAY

        var text = ParseExpression();
        Expression? row = null, col = null;

        if (Check(TokenType.At))
        {
            Advance(); // consume AT
            row = ParseExpression();
            if (Check(TokenType.Comma))
            {
                Advance();
                col = ParseExpression();
            }
        }

        return new SayStmt(text, row, col, line);
    }

    private MessageStmt ParseMessage()
    {
        int line = Current.Line;
        Advance();
        var text = ParseExpression();
        bool nowait = false;
        Expression? windowsParam = null;
        // Parse optional qualifiers: NOWAIT, WIN <expr>
        while (!IsAtEndOfStatement)
        {
            string val = Current.Value.ToUpperInvariant();
            if (val == "NOWAIT") { nowait = true; Advance(); }
            else if (val == "WIN") { Advance(); windowsParam = ParseExpression(); }
            else { Advance(); } // skip unknown qualifiers
        }
        return new MessageStmt(text, line, nowait, windowsParam);
    }

    private QuitStmt ParseQuit()
    {
        int line = Current.Line;
        Advance();
        return new QuitStmt(line);
    }

    private ClearScreenStmt ParseClearScreen()
    {
        int line = Current.Line;
        Advance();
        return new ClearScreenStmt(line);
    }

    private ExitStmt ParseExit()
    {
        int line = Current.Line;
        Advance(); // consume EXIT/EXIT_IF/FEXIT/FEXIT_IF/SEXIT/SEXIT_IF
        SkipToEndOfLine(); // skip optional condition
        return new ExitStmt(line);
    }

    private LoopStmt ParseLoop()
    {
        int line = Current.Line;
        Advance(); // consume LOOP/LOOP_IF/FLOOP/FLOOP_IF/SLOOP/SLOOP_IF
        SkipToEndOfLine(); // skip optional condition
        return new LoopStmt(line);
    }

    private Statement ParseIdentifierStatement()
    {
        // Look ahead for assignment: identifier = expr  or  identifier(index) = expr
        if (IsAssignment())
            return ParseAssignment();

        // Otherwise: this identifier is a command name we don't know about
        // Treat it as a generic command and consume the rest of the line
        return ParseGenericCommand();
    }

    private bool IsAssignment()
    {
        int saved = _pos;
        try
        {
            Advance(); // identifier
            // Array access: identifier(index) = ...
            if (Check(TokenType.LeftParen))
            {
                int depth = 0;
                while (!IsAtEnd && !IsAtEndOfStatement)
                {
                    if (Check(TokenType.LeftParen)) { depth++; Advance(); }
                    else if (Check(TokenType.RightParen)) { depth--; Advance(); if (depth == 0) break; }
                    else Advance();
                }
            }
            return Check(TokenType.Equal);
        }
        finally
        {
            _pos = saved;
        }
    }

    private AssignmentStmt ParseAssignment()
    {
        int line = Current.Line;
        string name = Advance().Value; // identifier
        Expression? index = null;

        if (Check(TokenType.LeftParen))
        {
            Advance(); // (
            index = ParseExpression();
            Expect(TokenType.RightParen, "')'");
        }

        Expect(TokenType.Equal, "'='");
        var value = ParseExpression();

        return new AssignmentStmt(name, index, value, line);
    }

    private GenericCommandStmt ParseGenericCommand()
    {
        int line = Current.Line;
        string commandName = Current.Value;
        Advance(); // consume the command name token

        var tokens = CollectRestOfLine();

        return new GenericCommandStmt(commandName, tokens, line);
    }

    // --- Expression parsing (Pratt-style precedence climbing) ---

    public Expression ParseExpression(int minPrecedence = 0)
    {
        var left = ParseUnary();

        while (!IsAtEnd && !IsAtEndOfExpression)
        {
            var (op, prec) = GetBinaryOp(Current);
            if (op == null || prec < minPrecedence) break;

            Advance();
            var right = ParseExpression(prec + 1);
            left = new BinaryExpr(left, op, right, left.Line);
        }

        return left;
    }

    private Expression ParseUnary()
    {
        if (Check(TokenType.Not))
        {
            var op = Advance();
            var operand = ParseUnary();
            return new UnaryExpr(".NOT.", operand, op.Line);
        }
        if (Check(TokenType.Minus))
        {
            var op = Advance();
            var operand = ParseUnary();
            return new UnaryExpr("-", operand, op.Line);
        }
        return ParseAtom();
    }

    private Expression ParseAtom()
    {
        var token = Current;

        switch (token.Type)
        {
            case TokenType.IntegerLiteral:
                Advance();
                return new LiteralExpr(int.Parse(token.Value), token.Line);

            case TokenType.NumericLiteral:
                Advance();
                return new LiteralExpr(double.Parse(token.Value), token.Line);

            case TokenType.StringLiteral:
                Advance();
                return new LiteralExpr(token.Value, token.Line);

            case TokenType.True:
                Advance();
                return new LiteralExpr(true, token.Line);

            case TokenType.False:
                Advance();
                return new LiteralExpr(false, token.Line);

            case TokenType.LeftParen:
                Advance();
                var expr = ParseExpression();
                Expect(TokenType.RightParen, "')'");
                return expr;

            case TokenType.Identifier:
                return ParseIdentifierExpr();

            // Keywords that can appear as function names in expressions
            case TokenType.Ask:
            case TokenType.Enter:
            case TokenType.Type:
            case TokenType.Size:
            case TokenType.Array:
            case TokenType.At:
                return ParseIdentifierExpr();

            default:
                throw new ParseException($"Unexpected token {token.Type}({token.Value}) at line {token.Line}:{token.Column}");
        }
    }

    private Expression ParseIdentifierExpr()
    {
        var token = Advance();
        string name = token.Value;

        // Function call or array access: name(args...)
        if (Check(TokenType.LeftParen))
        {
            Advance(); // (
            var args = new List<Expression>();
            if (!Check(TokenType.RightParen))
            {
                args.Add(ParseExpression());
                while (Check(TokenType.Comma))
                {
                    Advance();
                    if (!Check(TokenType.RightParen))
                        args.Add(ParseExpression());
                }
            }
            Expect(TokenType.RightParen, "')'");
            return new FunctionCallExpr(name, args, token.Line);
        }

        return new IdentifierExpr(name, token.Line);
    }

    private static (string? op, int precedence) GetBinaryOp(Token token)
    {
        return token.Type switch
        {
            TokenType.Or => (".OR.", 1),
            TokenType.And => (".AND.", 2),
            TokenType.Not => (".NOT.", 2), // .NOT. between expressions = .AND. .NOT.
            TokenType.Equal => ("=", 3),
            TokenType.NotEqual => ("<>", 3),
            TokenType.LessThan => ("<", 3),
            TokenType.LessEqual => ("<=", 3),
            TokenType.GreaterThan => (">", 3),
            TokenType.GreaterEqual => (">=", 3),
            TokenType.Plus => ("+", 4),
            TokenType.Minus => ("-", 4),
            TokenType.Star => ("*", 5),
            TokenType.Slash => ("/", 5),
            TokenType.Dollar => ("$", 3), // substring containment operator in TAS
            _ => (null, 0),
        };
    }

    /// <summary>
    /// Parses statements inside a block body with error recovery.
    /// On ParseException, skips to end of line and continues.
    /// Returns false if no progress was made (caller should break).
    /// </summary>
    private bool ParseBlockStatement(List<Statement> body)
    {
        int before = _pos;
        try
        {
            var stmt = ParseStatement();
            if (stmt != null) body.Add(stmt);
        }
        catch (ParseException)
        {
            SkipToEndOfLine();
        }
        SkipNewlines();
        return _pos != before;
    }

    // --- Helper methods ---

    private Token Current => _pos < _tokens.Count ? _tokens[_pos] : _tokens[^1];
    private bool IsAtEnd => _pos >= _tokens.Count || Current.Type == TokenType.Eof;

    private bool IsAtEndOfStatement =>
        IsAtEnd || Current.Type == TokenType.Newline || Current.Type == TokenType.Eof;

    /// <summary>
    /// Expressions end at newlines, but also at keywords that start new clauses
    /// like AT, THEN, WINDOWS, DFLT, etc.
    /// </summary>
    private bool IsAtEndOfExpression =>
        IsAtEndOfStatement
        || Current.Type == TokenType.At
        || Current.Type == TokenType.Then;

    private bool Check(TokenType type) => !IsAtEnd && Current.Type == type;

    private Token Advance()
    {
        var token = Current;
        _pos++;
        return token;
    }

    private void SkipNewlines()
    {
        while (Check(TokenType.Newline)) Advance();
    }

    private Statement? SkipNewlinesAndReturnNull()
    {
        SkipNewlines();
        return null;
    }

    private void SkipToEndOfLine()
    {
        while (!IsAtEndOfStatement)
            Advance();
    }

    private List<Token> CollectRestOfLine()
    {
        var tokens = new List<Token>();
        while (!IsAtEndOfStatement)
            tokens.Add(Advance());
        return tokens;
    }

    private Token Expect(TokenType type, string description)
    {
        if (Check(type)) return Advance();
        throw new ParseException($"Expected {description} but got {Current.Type}({Current.Value}) at line {Current.Line}:{Current.Column}");
    }

    private void ExpectSeparator()
    {
        if (Check(TokenType.Semicolon)) { Advance(); return; }
        if (Check(TokenType.Comma)) { Advance(); return; }
        // Skip any token that looks like a separator
        if (!IsAtEnd && !Check(TokenType.RightParen))
            Advance();
    }

    private string ExpectAnyIdentifier(string context)
    {
        // Accept Identifier or any keyword token as an identifier (TAS allows keywords as field names)
        if (Check(TokenType.Identifier)) return Advance().Value;
        // Keywords that commonly appear as identifiers
        var t = Current.Type;
        if (t >= TokenType.Define && t <= TokenType.Quit)
            return Advance().Value;
        throw new ParseException($"Expected {context} (identifier) but got {Current.Type}({Current.Value}) at line {Current.Line}:{Current.Column}");
    }

    private int ExpectInteger(string context)
    {
        if (Check(TokenType.IntegerLiteral))
            return int.Parse(Advance().Value);
        throw new ParseException($"Expected {context} (integer) but got {Current.Type}({Current.Value}) at line {Current.Line}:{Current.Column}");
    }
}

public class ParseException : Exception
{
    public ParseException(string message) : base(message) { }
}
