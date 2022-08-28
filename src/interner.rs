use std::cell::{Ref, RefCell};

use lasso::Rodeo;
use magic_static::magic_static;

use crate::data_structures::Sym;

#[magic_static]
pub static INTERNER: RefCell<Rodeo> = RefCell::new(Rodeo::default());

impl<S> From<S> for Sym
where
    S: AsRef<str>,
{
    fn from(s: S) -> Self {
        Sym(INTERNER.borrow_mut().get_or_intern(s.as_ref()))
    }
}

impl<'str_ref, 'sym_ref> From<&'sym_ref Sym> for Ref<'str_ref, str>
where
    'sym_ref: 'str_ref,
    // 'str_ref: 'sym_ref,
{
    fn from(sym: &Sym) -> Self {
        Ref::map(INTERNER.borrow(), |i| i.resolve(&sym.0))
    }
}

impl Sym {
    pub fn to_str<'str_ref, 'sym_ref>(&'sym_ref self) -> Ref<'str_ref, str>
    where
        'sym_ref: 'str_ref,
    {
        self.into()
    }
}
