namespace CoTAS.Parser;

public enum TokenType
{
    // Literals
    StringLiteral,
    IntegerLiteral,
    NumericLiteral,
    Identifier,

    // Keywords - field definition
    Define,
    Type,
    Size,
    Dec,
    Array,
    Reset,

    // Keywords - control flow
    If,
    Then,
    Else,
    ElseIf,
    EndIf,
    While,
    EndW,
    For,
    Next,
    Goto,
    Gosub,
    Ret,
    Select,
    Case,
    Otherwise,
    EndC,
    Scan,
    EndS,

    // Keywords - loop control
    Exit,
    ExitIf,
    Loop,
    LoopIf,
    Floop,
    FloopIf,
    Fexit,
    FexitIf,
    Sloop,
    SloopIf,
    Sexit,
    SexitIf,

    // Keywords - I/O and display
    Say,
    At,
    Msg,
    Enter,
    Ask,
    ClrScr,

    // Keywords - misc commands (parsed as generic)
    Quit,

    // Logical constants
    True,       // .T.
    False,      // .F.

    // Logical operators
    And,        // .A. / .AND.
    Or,         // .O. / .OR.
    Not,        // .N. / .NOT.

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    NotEqual,   // <>
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Bang,       // !
    Dollar,     // $
    Pipe,       // |
    Backslash,  // \.
    Question,   // ?
    Tilde,      // ~
    Percent,    // %
    Ampersand,  // & (single)
    Caret,      // ^

    // Delimiters
    Comma,
    Semicolon,
    Colon,
    Dot,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    AtSign,

    // Special
    Newline,
    Comment,
    Preprocessor,
    Label,
    Eof,
}
