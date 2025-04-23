enum TokenType
{
    Eof,
    Illegal,

    // Core
    Let,
    Const,
    Struct,
    Function,
    If,
    Else,
    Switch,
    Case,
    Break,
    Return,
    While,
    For,
    And,
    Or,
    In,

    // Types
    Identifier,
    Number,
    Float,
    Boolean,
    String,
    Char,

    // Datatypes
    Int,
    F32,
    Bool,
    Str,
    Nil,
    Void,

    // Single Characters
    Exclamation, // !
    At, // @
    Hashtag, // #
    Dollar, // $
    Percent, // %
    Caret, // ^
    Ampersand, // &
    Asterisk, // *
    Lparen, // (
    Rparen, // )
    Minus, // -
    Underscore, // _
    Plus, // +
    Assign, // =
    Lbracket, // [
    Rbracket, // ]
    Lbrace, // {
    Rbrace, // }
    Semicolon, // ;
    Colon, // :
    Apostrophe, // '
    Quote, // "
    Comma, // ,
    Period, // .
    Lessthan, // <
    Greaterthan, // >
    Slash, // /
    Question, // ?
    Backslash, // \/
    Pipe, // |

    // Double Characters
    Equal, // ==
    NotEqual, // !=
    PlusEq, // +=
    MinusEq, // -=
    MultEq, // *=
    DivEq, // /=
    LtEqual, // <=
    GtEqual, // >=
    Increment, // ++
    Decrement, // --
    Comment, // //
}

struct Token
{
    TokenType type;
    string literal;
}

TokenType[string] IDENTIFIER_MAP = [
    "let": TokenType.Let,
    "const": TokenType.Const,
    "struct": TokenType.Struct,
    "fn": TokenType.Function,
    "if": TokenType.If,
    "else": TokenType.Else,
    "switch": TokenType.Switch,
    "case": TokenType.Case,
    "break": TokenType.Break,
    "return": TokenType.Return,
    "while": TokenType.While,
    "for": TokenType.For,
    "and": TokenType.And,
    "or": TokenType.Or,
    "in": TokenType.In,

    "true": TokenType.Boolean,
    "false": TokenType.Boolean,

    "bool": TokenType.Bool,
    "int": TokenType.Int,
    "f32": TokenType.F32,
    "str": TokenType.Str,
    "nil": TokenType.Nil,
    "void": TokenType.Void,
];

TokenType[string] DOUBLE_TOKEN_MAP = [
    "==": TokenType.Equal,
    "!=": TokenType.NotEqual,
    "+=": TokenType.PlusEq,
    "-=": TokenType.MinusEq,
    "*=": TokenType.MultEq,
    "/=": TokenType.DivEq,
    "<=": TokenType.LtEqual,
    ">=": TokenType.GtEqual,
    "++": TokenType.Increment,
    "--": TokenType.Decrement,
    "//": TokenType.Comment,
];
