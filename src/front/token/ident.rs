use super::{RichErr, Token};

use chumsky::prelude::*;

pub fn ident<'src>() -> impl Parser<'src, &'src str, Token<'src>, RichErr<'src>> {
    let underscore_start = just('_')
        .then(any().filter(|c| unicode_ident::is_xid_continue(*c) && *c != '_'))
        .then(
            any()
                .filter(|c| unicode_ident::is_xid_continue(*c))
                .repeated()
                .or_not(),
        )
        .to_slice();

    let normal_start = any()
        .filter(|c| unicode_ident::is_xid_start(*c))
        .then(
            any()
                .filter(|c| unicode_ident::is_xid_continue(*c))
                .repeated()
                .or_not(),
        )
        .to_slice();

    choice((underscore_start, normal_start)).map(Token::Ident)
}
