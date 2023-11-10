use std::{
    fmt::{Debug, Display, Formatter},
    ops::{Deref, DerefMut},
};

use chumsky::span::{SimpleSpan, Span};

pub trait SpanAble: Sized {
    fn with_span(self, span: SimpleSpan<usize>) -> WithSpan<Self> {
        WithSpan(span, self)
    }
}

impl<T: Sized> SpanAble for T {}

impl<T: SpanAble + Display> Display for WithSpan<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.1)
    }
}

impl<T: SpanAble + Debug> Debug for WithSpan<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?}, {:?})", self.1, self.0)
    }
}

impl<T: SpanAble + Eq> Eq for WithSpan<T> {}
impl<T: SpanAble + PartialEq> PartialEq for WithSpan<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0 && self.1 == other.1
    }
}

pub struct WithSpan<T: SpanAble>(SimpleSpan<usize>, T);

impl<T: SpanAble> WithSpan<T> {
    fn span(&self) -> SimpleSpan<usize> {
        self.0
    }

    fn as_inner(&self) -> &T {
        &self.1
    }

    fn into_inner(self) -> T {
        self.1
    }
}
