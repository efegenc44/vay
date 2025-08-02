use crate::interner::InternIdx;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Token {
    Identifier(InternIdx),
    U64(u64),

    Semicolon,
    Colon,
    DoubleColon,
    Comma,
    LeftParenthesis,
    RightParenthesis,
    LeftCurly,
    RightCurly,
    Dot,
    Equals,

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
}

impl Token {
    pub const fn dummy_identifier() -> Token {
        Token::Identifier(InternIdx::dummy_idx())
    }

    pub fn kind_name(&self) -> &str {
        match self {
            Token::Identifier(_) => "an identifier",
            Token::U64(_) => "a u64 number",

            Token::Semicolon => "`;`",
            Token::Colon => "`:`",
            Token::DoubleColon => "`::`",
            Token::Comma => "`,`",
            Token::LeftParenthesis => "`(`",
            Token::RightParenthesis => "`)`",
            Token::LeftCurly => "`{`",
            Token::RightCurly => "`}`",
            Token::Dot => "`.`",
            Token::Equals => "`=`",

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
        }
    }
}
