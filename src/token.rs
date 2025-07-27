use crate::interner::InternIdx;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Token {
    Identifier(InternIdx),

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
    ProcKeyword,
    VariantKeyword,
    ReturnKeyword,
    MatchKeyword,
    InterfaceKeyword,
    LetKeyword,
    InKeyword,
}

impl Token {
    pub const fn dummy_identifier() -> Token {
        Token::Identifier(InternIdx::dummy_idx())
    }

    pub fn kind_name(&self) -> &str {
        match self {
            Token::Identifier(_) => "an identifier",

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
            Token::ProcKeyword => "keyword `proc`",
            Token::VariantKeyword => "keyword `variant`",
            Token::ReturnKeyword => "keyword `return`",
            Token::MatchKeyword => "keyword `match`",
            Token::InterfaceKeyword => "keyword `interface`",
            Token::LetKeyword => "keyword `let`",
            Token::InKeyword => "keyword `in`",
        }
    }
}
