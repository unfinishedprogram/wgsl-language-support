use logos::Logos;
mod keyword;
use keyword::{parse_ident, IdentError, Keyword};

#[derive(Debug, Clone, PartialEq, Default)]
pub enum LexError {
    InvalidIdentifier(IdentError),
    #[default]
    Other,
}

impl From<IdentError> for LexError {
    fn from(err: IdentError) -> Self {
        LexError::InvalidIdentifier(err)
    }
}

#[derive(Logos, Debug, PartialEq, Eq, Clone)]
#[logos(error = LexError)]
#[logos(skip r"\s")]
pub enum Token<'src> {
    #[token("alias", |_| Keyword::Alias)]
    #[token("break", |_| Keyword::Break)]
    #[token("case", |_| Keyword::Case)]
    #[token("const", |_| Keyword::Const)]
    #[token("const_assert", |_| Keyword::ConstAssert)]
    #[token("continue", |_| Keyword::Continue)]
    #[token("continuing", |_| Keyword::Continuing)]
    #[token("default", |_| Keyword::Default)]
    #[token("diagnostic", |_| Keyword::Diagnostic)]
    #[token("discard", |_| Keyword::Discard)]
    #[token("else", |_| Keyword::Else)]
    #[token("enable", |_| Keyword::Enable)]
    #[token("fn", |_| Keyword::Fn)]
    #[token("for", |_| Keyword::For)]
    #[token("if", |_| Keyword::If)]
    #[token("let", |_| Keyword::Let)]
    #[token("loop", |_| Keyword::Loop)]
    #[token("override", |_| Keyword::Override)]
    #[token("requires", |_| Keyword::Requires)]
    #[token("return", |_| Keyword::Return)]
    #[token("struct", |_| Keyword::Struct)]
    #[token("switch", |_| Keyword::Switch)]
    #[token("var", |_| Keyword::Var)]
    #[token("while", |_| Keyword::While)]
    Keyword(Keyword),

    #[regex(r"([_\p{XID_Start}][\p{XID_Continue}]+)|([\p{XID_Start}])|_", |lex| parse_ident(lex.slice()))]
    Ident(&'src str),
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    pub fn ident_with_leading_underscore_is_valid() {
        let ident = "_validIdent";
        let mut lexer = Token::lexer(ident);
        assert_eq!(
            Some(Ok(Token::Ident(ident))),
            lexer.next(),
            "Leading underscore should be a valid token"
        );
    }

    #[test]
    pub fn ident_with_double_leading_underscore_is_invalid() {
        let ident = "__invalidIdent";
        let mut lexer = Token::lexer(ident);
        assert_eq!(
            Some(Err(LexError::InvalidIdentifier(
                IdentError::DoubleLeadingUnderscore
            ))),
            lexer.next(),
            "Double leading underscore should not be valid"
        );
    }

    #[test]
    pub fn ident_with_single_underscore_is_invalid() {
        let ident = "_";
        let mut lexer = Token::lexer(ident);
        assert_eq!(
            Some(Err(LexError::InvalidIdentifier(
                IdentError::SingleUnderscore
            ))),
            lexer.next(),
            "Single underscore should not be valid ident"
        );
    }

    #[test]
    pub fn ident_with_non_ascii_is_valid() {
        // Examples taken from WGSL language specification
        let idents = [
            "Δέλτα",
            "réflexion",
            "Кызыл",
            "𐰓𐰏𐰇",
            "朝焼け",
            "سلام",
            "검정",
            "שָׁלוֹם",
            "गुलाबी",
            "փիրուզ",
        ];

        for ident in idents.iter() {
            let mut lexer = Token::lexer(ident);
            assert_eq!(
                Some(Ok(Token::Ident(ident))),
                lexer.next(),
                "Failed for {}",
                ident
            );
        }
    }

    #[test]
    pub fn keyword_takes_priority() {
        let keywords = [
            ("alias", Keyword::Alias),
            ("break", Keyword::Break),
            ("case", Keyword::Case),
            ("const", Keyword::Const),
            ("const_assert", Keyword::ConstAssert),
            ("continue", Keyword::Continue),
            ("continuing", Keyword::Continuing),
            ("default", Keyword::Default),
            ("diagnostic", Keyword::Diagnostic),
            ("discard", Keyword::Discard),
            ("else", Keyword::Else),
            ("enable", Keyword::Enable),
            ("fn", Keyword::Fn),
            ("for", Keyword::For),
            ("if", Keyword::If),
            ("let", Keyword::Let),
            ("loop", Keyword::Loop),
            ("override", Keyword::Override),
            ("requires", Keyword::Requires),
            ("return", Keyword::Return),
            ("struct", Keyword::Struct),
            ("switch", Keyword::Switch),
            ("var", Keyword::Var),
            ("while", Keyword::While),
        ];
        for (source, keyword) in keywords.iter() {
            let mut lexer = Token::lexer(source);
            assert_eq!(
                Some(Ok(Token::Keyword(*keyword))),
                lexer.next(),
                "Lexer should prioritize keyword {:?} over ident",
                keyword
            );
        }
    }

    #[test]
    pub fn reserved_words_should_be_invalid() {
        let ident = "interface";
        let mut lexer = Token::lexer(ident);
        assert_eq!(
            Some(Err(LexError::InvalidIdentifier(
                IdentError::ReservedKeyword
            ))),
            lexer.next(),
            "Reserved words should not be valid"
        );
    }
}
