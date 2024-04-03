use std::{
    cell::{Ref, RefCell},
    fmt,
};

use lasso::Rodeo;
use magic_static::magic_static;

#[magic_static]
pub static INTERNER: RefCell<Rodeo> = RefCell::new(Rodeo::default());

/// An interned string.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct IStr(pub(crate) lasso::Spur);

impl fmt::Debug for IStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Sym({})", self.to_str())
    }
}

impl fmt::Display for IStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

impl<S> From<S> for IStr
where
    S: AsRef<str>,
{
    fn from(s: S) -> Self {
        IStr(INTERNER.borrow_mut().get_or_intern(s.as_ref()))
    }
}

impl<'str_ref, 'sym_ref> From<&'sym_ref IStr> for Ref<'str_ref, str>
where
    'sym_ref: 'str_ref,
{
    fn from(s: &IStr) -> Self {
        Ref::map(INTERNER.borrow(), |i| i.resolve(&s.0))
    }
}

impl IStr {
    pub fn to_str<'str_ref, 'sym_ref>(&'sym_ref self) -> Ref<'str_ref, str>
    where
        'sym_ref: 'str_ref,
    {
        self.into()
    }

    pub fn is_one_char(&self) -> bool {
        let s = self.to_str();
        let mut cs = s.chars();
        cs.next().is_some_and(|_| cs.next().is_none())
    }
}
