using System.Text;

namespace CoTAS.Parser;

public sealed class Lexer
{
    private readonly string _source;
    private int _pos;
    private int _line = 1;
    private int _col = 1;
    private bool _atLineStart = true;
    private int _parenDepth;

    private static readonly Dictionary<string, TokenType> Keywords = new(StringComparer.OrdinalIgnoreCase)
    {
        // Field definition
        ["define"] = TokenType.Define,
        ["def"] = TokenType.Define,
        ["type"] = TokenType.Type,
        ["size"] = TokenType.Size,
        ["dec"] = TokenType.Dec,
        ["array"] = TokenType.Array,
        ["reset"] = TokenType.Reset,

        // Control flow
        ["if"] = TokenType.If,
        ["then"] = TokenType.Then,
        ["else"] = TokenType.Else,
        ["else_if"] = TokenType.ElseIf,
        ["endif"] = TokenType.EndIf,
        ["while"] = TokenType.While,
        ["endw"] = TokenType.EndW,
        ["for"] = TokenType.For,
        ["next"] = TokenType.Next,
        ["goto"] = TokenType.Goto,
        ["gosub"] = TokenType.Gosub,
        ["ret"] = TokenType.Ret,
        ["return"] = TokenType.Ret,
        ["select"] = TokenType.Select,
        ["case"] = TokenType.Case,
        ["otherwise"] = TokenType.Otherwise,
        ["endc"] = TokenType.EndC,
        ["scan"] = TokenType.Scan,
        ["ends"] = TokenType.EndS,
        ["endscan"] = TokenType.EndS,

        // Loop control
        ["exit"] = TokenType.Exit,
        ["exit_if"] = TokenType.ExitIf,
        ["loop"] = TokenType.Loop,
        ["loop_if"] = TokenType.LoopIf,
        ["floop"] = TokenType.Floop,
        ["floop_if"] = TokenType.FloopIf,
        ["fexit"] = TokenType.Fexit,
        ["fexit_if"] = TokenType.FexitIf,
        ["sloop"] = TokenType.Sloop,
        ["sloop_if"] = TokenType.SloopIf,
        ["sexit"] = TokenType.Sexit,
        ["sexit_if"] = TokenType.SexitIf,

        // I/O and display
        ["say"] = TokenType.Say,
        ["at"] = TokenType.At,
        ["msg"] = TokenType.Msg,
        ["enter"] = TokenType.Enter,
        ["ask"] = TokenType.Ask,
        ["clrscr"] = TokenType.ClrScr,

        // Misc
        ["quit"] = TokenType.Quit,
    };

    public Lexer(string source)
    {
        _source = source ?? throw new ArgumentNullException(nameof(source));
    }

    public List<Token> Tokenize()
    {
        var tokens = new List<Token>();

        while (!IsAtEnd)
        {
            SkipWhitespace();
            if (IsAtEnd) break;

            var token = NextToken();
            if (token != null)
                tokens.Add(token);
        }

        tokens.Add(new Token(TokenType.Eof, "", _line, _col));
        return tokens;
    }

    private Token? NextToken()
    {
        char c = Peek();

        // Newline
        if (c == '\r' || c == '\n')
            return ReadNewline();

        // Comment: ; or && or * at start of line
        // ; inside parentheses is a FOR separator, not a comment
        if ((c == ';' && _parenDepth == 0) || (c == '&' && PeekNext() == '&') || (c == '*' && _atLineStart))
            return ReadComment();

        // Semicolon inside parens = separator token
        if (c == ';' && _parenDepth > 0)
        {
            _atLineStart = false;
            Advance();
            return new Token(TokenType.Semicolon, ";", _line, _col - 1);
        }

        // Preprocessor: # at start of line
        if (c == '#' && _atLineStart)
            return ReadPreprocessor();

        _atLineStart = false;

        // Strings
        if (c == '\'' || c == '"')
            return ReadString();

        // Dot-prefixed logical constants/operators: .T. .F. .A. .AND. .O. .OR. .N. .NOT.
        if (c == '.' && _pos + 1 < _source.Length && char.IsLetter(_source[_pos + 1]))
        {
            var dotToken = TryReadDotToken();
            if (dotToken != null) return dotToken;
        }

        // Numbers
        if (char.IsDigit(c) || (c == '.' && _pos + 1 < _source.Length && char.IsDigit(_source[_pos + 1])))
            return ReadNumber();

        // Identifiers / keywords / labels
        if (char.IsLetter(c) || c == '_')
            return ReadIdentifierOrKeyword();

        // Operators and delimiters
        return ReadOperatorOrDelimiter();
    }

    private Token ReadNewline()
    {
        int startLine = _line, startCol = _col;
        if (Peek() == '\r') Advance();
        if (!IsAtEnd && Peek() == '\n') Advance();
        _line++;
        _col = 1;
        _atLineStart = true;
        // Don't reset paren depth here - TAS FOR can span multiple lines inside parens?
        // Actually FOR args are always on one line, but let the parser handle mismatches.
        return new Token(TokenType.Newline, "\\n", startLine, startCol);
    }

    private Token ReadComment()
    {
        int startLine = _line, startCol = _col;
        var sb = new StringBuilder();
        while (!IsAtEnd && Peek() != '\r' && Peek() != '\n')
            sb.Append(Advance());
        return new Token(TokenType.Comment, sb.ToString(), startLine, startCol);
    }

    private Token ReadPreprocessor()
    {
        int startLine = _line, startCol = _col;
        var sb = new StringBuilder();
        while (!IsAtEnd && Peek() != '\r' && Peek() != '\n')
            sb.Append(Advance());
        return new Token(TokenType.Preprocessor, sb.ToString(), startLine, startCol);
    }

    private Token ReadString()
    {
        int startLine = _line, startCol = _col;
        char quote = Advance();
        var sb = new StringBuilder();
        while (!IsAtEnd && Peek() != quote)
        {
            if (Peek() == '\r' || Peek() == '\n') break; // unterminated string
            sb.Append(Advance());
        }
        if (!IsAtEnd && Peek() == quote) Advance();
        return new Token(TokenType.StringLiteral, sb.ToString(), startLine, startCol);
    }

    private Token? TryReadDotToken()
    {
        int startLine = _line, startCol = _col;

        // Try to match .X. patterns
        int i = _pos + 1;
        var sb = new StringBuilder();
        while (i < _source.Length && char.IsLetter(_source[i]))
        {
            sb.Append(_source[i]);
            i++;
        }
        if (i < _source.Length && _source[i] == '.')
        {
            string inner = sb.ToString().ToUpperInvariant();
            TokenType? type = inner switch
            {
                "T" => TokenType.True,
                "F" => TokenType.False,
                "A" or "AND" => TokenType.And,
                "O" or "OR" => TokenType.Or,
                "N" or "NOT" => TokenType.Not,
                _ => null
            };
            if (type != null)
            {
                string full = _source.Substring(_pos, i - _pos + 1);
                _pos = i + 1;
                _col += full.Length;
                return new Token(type.Value, full, startLine, startCol);
            }
        }
        return null; // not a dot token, let caller handle the dot
    }

    private Token ReadNumber()
    {
        int startLine = _line, startCol = _col;
        var sb = new StringBuilder();
        bool hasDot = false;

        while (!IsAtEnd && (char.IsDigit(Peek()) || Peek() == '.'))
        {
            if (Peek() == '.')
            {
                if (hasDot) break;
                // Make sure next char is a digit (not .T. etc.)
                if (_pos + 1 < _source.Length && char.IsDigit(_source[_pos + 1]))
                    hasDot = true;
                else
                    break;
            }
            sb.Append(Advance());
        }

        return new Token(
            hasDot ? TokenType.NumericLiteral : TokenType.IntegerLiteral,
            sb.ToString(), startLine, startCol);
    }

    private Token ReadIdentifierOrKeyword()
    {
        int startLine = _line, startCol = _col;
        var sb = new StringBuilder();

        while (!IsAtEnd && (char.IsLetterOrDigit(Peek()) || Peek() == '_' || Peek() == '.'
            || (Peek() == '@' && sb.Length > 0))) // Allow @ mid-identifier for FUNC@21, CONST@offset patterns
        {
            // Allow dots in identifiers like DICT.KEY but not .T. style
            if (Peek() == '.')
            {
                if (_pos + 1 < _source.Length && (char.IsLetterOrDigit(_source[_pos + 1]) || _source[_pos + 1] == '_'))
                    sb.Append(Advance());
                else
                    break;
            }
            else
            {
                sb.Append(Advance());
            }
        }

        string word = sb.ToString();

        // Check if this is a label (identifier followed by colon at end or before whitespace)
        if (!IsAtEnd && Peek() == ':')
        {
            Advance(); // consume the colon
            return new Token(TokenType.Label, word, startLine, startCol);
        }

        // Check keywords
        if (Keywords.TryGetValue(word, out var kwType))
            return new Token(kwType, word, startLine, startCol);

        return new Token(TokenType.Identifier, word, startLine, startCol);
    }

    private Token ReadOperatorOrDelimiter()
    {
        int startLine = _line, startCol = _col;
        char c = Advance();

        return c switch
        {
            '+' => new Token(TokenType.Plus, "+", startLine, startCol),
            '-' => new Token(TokenType.Minus, "-", startLine, startCol),
            '*' => new Token(TokenType.Star, "*", startLine, startCol),
            '/' => new Token(TokenType.Slash, "/", startLine, startCol),
            '=' => new Token(TokenType.Equal, "=", startLine, startCol),
            '<' => !IsAtEnd && Peek() == '>'
                ? (Advance(), new Token(TokenType.NotEqual, "<>", startLine, startCol)).Item2
                : !IsAtEnd && Peek() == '='
                    ? (Advance(), new Token(TokenType.LessEqual, "<=", startLine, startCol)).Item2
                    : new Token(TokenType.LessThan, "<", startLine, startCol),
            '>' => !IsAtEnd && Peek() == '='
                ? (Advance(), new Token(TokenType.GreaterEqual, ">=", startLine, startCol)).Item2
                : new Token(TokenType.GreaterThan, ">", startLine, startCol),
            ',' => new Token(TokenType.Comma, ",", startLine, startCol),
            ':' => new Token(TokenType.Colon, ":", startLine, startCol),
            '.' => new Token(TokenType.Dot, ".", startLine, startCol),
            '(' => IncrementParenDepth(startLine, startCol),
            ')' => DecrementParenDepth(startLine, startCol),
            '[' => new Token(TokenType.LeftBracket, "[", startLine, startCol),
            ']' => new Token(TokenType.RightBracket, "]", startLine, startCol),
            '{' => new Token(TokenType.LeftBrace, "{", startLine, startCol),
            '}' => new Token(TokenType.RightBrace, "}", startLine, startCol),
            '@' => new Token(TokenType.AtSign, "@", startLine, startCol),
            '#' => new Token(TokenType.Preprocessor, "#", startLine, startCol),
            '!' => new Token(TokenType.Bang, "!", startLine, startCol),
            '$' => new Token(TokenType.Dollar, "$", startLine, startCol),
            '|' => new Token(TokenType.Pipe, "|", startLine, startCol),
            '\\' => new Token(TokenType.Backslash, "\\", startLine, startCol),
            '?' => new Token(TokenType.Question, "?", startLine, startCol),
            '~' => new Token(TokenType.Tilde, "~", startLine, startCol),
            '%' => new Token(TokenType.Percent, "%", startLine, startCol),
            '&' => new Token(TokenType.Ampersand, "&", startLine, startCol),
            '^' => new Token(TokenType.Caret, "^", startLine, startCol),
            // For any other character we haven't seen, just emit it as an identifier to avoid crashing
            _ => new Token(TokenType.Identifier, c.ToString(), startLine, startCol),
        };
    }

    private Token IncrementParenDepth(int line, int col)
    {
        _parenDepth++;
        return new Token(TokenType.LeftParen, "(", line, col);
    }

    private Token DecrementParenDepth(int line, int col)
    {
        if (_parenDepth > 0) _parenDepth--;
        return new Token(TokenType.RightParen, ")", line, col);
    }

    private void SkipWhitespace()
    {
        while (!IsAtEnd && Peek() is ' ' or '\t')
        {
            _col++;
            _pos++;
        }
    }

    private bool IsAtEnd => _pos >= _source.Length;
    private char Peek() => _source[_pos];
    private char PeekNext() => _pos + 1 < _source.Length ? _source[_pos + 1] : '\0';

    private char Advance()
    {
        char c = _source[_pos++];
        _col++;
        return c;
    }
}

public class LexerException : Exception
{
    public LexerException(string message) : base(message) { }
}
