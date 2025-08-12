use crate::interner::InternIdx;

#[derive(Clone, Copy, PartialEq)]
pub enum Token {
    Identifier(InternIdx),
    String(InternIdx),
    U64(u64),
    F32(f32),

    Semicolon,
    Colon,
    DoubleColon,
    Comma,
    LeftParenthesis,
    RightParenthesis,
    LeftSquare,
    RightSquare,
    LeftCurly,
    RightCurly,
    Dot,
    DoubleDot,
    Equals,
    DoubleEquals,
    Minus,
    Plus,
    Asterisk,
    Slash,
    SlashEquals,
    Less,
    LessEquals,
    Greater,
    GreaterEquals,
    Bar,
    Ampersand,

    ModuleKeyword,
    ImportKeyword,
    FunKeyword,
    VariantKeyword,
    MatchKeyword,
    InterfaceKeyword,
    LetKeyword,
    InKeyword,
    AsKeyword,
    ReturnKeyword,
    StructKeyword,
    BuiltInKeyword,
    ExternalKeyword,
}

impl Token {
    pub const fn dummy_identifier() -> Token {
        Token::Identifier(InternIdx::dummy_idx())
    }

    pub fn kind_name(&self) -> &str {
        match self {
            Token::Identifier(_) => "an identifier",
            Token::U64(_) => "a u64 number",
            Token::F32(_) => "a f32 number",
            Token::String(_) => "a string",

            Token::Semicolon => "`;`",
            Token::Colon => "`:`",
            Token::DoubleColon => "`::`",
            Token::Comma => "`,`",
            Token::LeftParenthesis => "`(`",
            Token::RightParenthesis => "`)`",
            Token::LeftSquare => "`[`",
            Token::RightSquare => "`]`",
            Token::LeftCurly => "`{`",
            Token::RightCurly => "`}`",
            Token::Dot => "`.`",
            Token::DoubleDot => "`..`",
            Token::Equals => "`=`",
            Token::DoubleEquals => "`==`",
            Token::Minus => "`-`",
            Token::Plus => "`+`",
            Token::Asterisk => "`*`",
            Token::Slash => "`/`",
            Token::SlashEquals => "`/=`",
            Token::Less => "`<`",
            Token::LessEquals => "`<=`",
            Token::Greater => "`>`",
            Token::GreaterEquals => "`>=`",
            Token::Bar => "`|`",
            Token::Ampersand => "`&`",

            Token::ModuleKeyword => "keyword `module`",
            Token::ImportKeyword => "keyword `import`",
            Token::FunKeyword => "keyword `fun`",
            Token::VariantKeyword => "keyword `variant`",
            Token::MatchKeyword => "keyword `match`",
            Token::InterfaceKeyword => "keyword `interface`",
            Token::LetKeyword => "keyword `let`",
            Token::InKeyword => "keyword `in`",
            Token::AsKeyword => "keyword `as`",
            Token::ReturnKeyword => "keyword `return`",
            Token::StructKeyword => "keyword `struct`",
            Token::BuiltInKeyword => "keyword `builtin`",
            Token::ExternalKeyword => "keyword `external`",
        }
    }
}
