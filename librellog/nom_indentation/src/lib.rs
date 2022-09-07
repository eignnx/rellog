//! # `nom_indentation`
//!
//! Provides an input type which allows for indentation sensitive parsing.
//!
//! ## Note: **I9n** Abbreviates **Indentation**
//! Throughout the crate, the word **indentation** has been abbreviated **i9n**.
//!
//! ## Use
//! ### Never Heard of `nom_locate`?
//! That's ok! This crate uses and extends the functionality of a crate called
//! [`nom_locate`](https://docs.rs/nom_locate/latest/nom_locate/index.html).
//! That crate provides a type called `LocatedSpan<I>` that you wrap your input
//! (`I`) in. When `LocatedSpan`s are passed to your `nom` parsers, line and
//! column information is tracked as parsing occurs.
//!
//! `todo!("[eignnx]: further explanation needed here")`
//!
//! ### Already Familiar with `nom_locate`?
//! Great! Just replace `nom_locate::LocatedSpan<I, X>` with
//! [`nom_indentation::LocatedSpan<I, X>`]! I'd recommend doing this as a type
//! alias.
//!
//! Instead of this:
//! ```ignore
//! type MyInput = nom_locate::LocatedSpan<MyUnderlyingInput, MyExtraState>;
//! ```
//! ...do this:
//! ```ignore
//! type MyInput = nom_indentation::LocatedSpan<MyUnderlyingInput, MyExtraState>;
//! ```
//! Then use `MyInput` everywhere you expect to recieve input.
//!
//! #### Note on Extra State
//! This crate's [`LocatedSpan`] uses `nom_locate::LocatedSpan`'s extra state
//! parameter to hold *reference indentation* information.
//!
//! Because of this, for you to access *your* extra state, use the *methods*
//! [`extra`], [`extra_ref`], and [`extra_mut`].
//!
//! Do **not** access the `extra` *field*.

use nom::{AsBytes, IResult};

pub struct GroupI9n<Extra> {
    group_i9n: usize,
    extra: Extra,
}

/// This is a type alias. For detailed documentation see [`nom_locate::LocatedSpan`](https://docs.rs/nom_locate/latest/nom_locate/struct.LocatedSpan.html)
///
/// We keep track of the reference indentation in the `X`-tra type argument.
pub type LocatedSpan<Input, Extra = ()> = nom_locate::LocatedSpan<Input, GroupI9n<Extra>>;

pub trait LocatedSpanExt<I, X> {
    fn group_i9n(&self) -> usize;
    fn group_i9n_mut(&mut self) -> &mut usize;
    fn extra(self) -> X;
    fn extra_ref(&self) -> &X;
    fn extra_mut(&mut self) -> &mut X;
}

impl<I, X> LocatedSpanExt<I, X> for LocatedSpan<I, X> {
    #[inline]
    fn group_i9n(&self) -> usize {
        self.extra.group_i9n
    }

    #[inline]
    fn group_i9n_mut(&mut self) -> &mut usize {
        &mut self.extra.group_i9n
    }

    #[inline]
    fn extra(self) -> X {
        self.extra.extra
    }

    #[inline]
    fn extra_ref(&self) -> &X {
        &self.extra.extra
    }

    #[inline]
    fn extra_mut(&mut self) -> &mut X {
        &mut self.extra.extra
    }
}

#[derive(Debug, Clone)]
pub struct WithI9n<T> {
    /// The indentation of the beginning of `self.value`.
    pub start_i9n: usize,
    /// The wrapped inner value.
    pub value: T,
}

/// This is essentially a trait alias which specializes `nom::Parser`.
pub trait I9nParser<Input, Output, Error, Extra = ()>:
    nom::Parser<LocatedSpan<Input, Extra>, WithI9n<Output>, Error>
{
}

impl<I, O, E, P, X> I9nParser<I, O, E, X> for P where
    P: nom::Parser<LocatedSpan<I, X>, WithI9n<O>, E>
{
}

pub fn i9n_group<Input, Output, Error, Extra>(
    mut p: impl I9nParser<Input, Output, Error, Extra>,
) -> impl I9nParser<Input, Output, Error, Extra>
where
    Input: AsBytes,
    LocatedSpan<Input, Extra>: Clone,
{
    move |mut i: LocatedSpan<Input, Extra>| -> IResult<_, _, Error> {
        let old_ref_i9n = i.group_i9n();
        let new_ref_i9n = i.get_column();
        *i.group_i9n_mut() = new_ref_i9n;
        let (mut i, o) = p.parse(i)?;
        *i.group_i9n_mut() = old_ref_i9n;
        Ok((i, o))
    }
}

pub trait I9nRelation<T: PartialEq + PartialOrd> {
    fn partial_cmp(&self, x: &T, y: &T) -> bool;
}

pub mod relations {
    use crate::I9nRelation;

    pub struct Eq;
    impl<T: PartialEq + PartialOrd> I9nRelation<T> for Eq {
        fn partial_cmp(&self, x: &T, y: &T) -> bool {
            x == y
        }
    }

    pub struct Gt;
    impl<T: PartialEq + PartialOrd> I9nRelation<T> for Gt {
        fn partial_cmp(&self, x: &T, y: &T) -> bool {
            x > y
        }
    }

    pub struct Gte;
    impl<T: PartialEq + PartialOrd> I9nRelation<T> for Gte {
        fn partial_cmp(&self, x: &T, y: &T) -> bool {
            x >= y
        }
    }

    pub struct Any;
    impl<T: PartialEq + PartialOrd> I9nRelation<T> for Any {
        fn partial_cmp(&self, _x: &T, _y: &T) -> bool {
            true
        }
    }
}

pub enum I9nError<'rel> {
    BadI9nRelativeToGroupI9n {
        rel: &'rel dyn I9nRelation<usize>,
        group_i9n: usize,
        actual_i9n: usize,
    },
}

pub fn ind_rel<'rel, Input, Output, Error, Extra>(
    mut p: impl I9nParser<Input, Output, Error, Extra> + 'rel,
    rel: &'rel dyn I9nRelation<usize>,
) -> impl I9nParser<Input, Output, Error, Extra> + 'rel
where
    Input: AsBytes,
    LocatedSpan<Input, Extra>: Clone,
    Error: From<(LocatedSpan<Input, Extra>, I9nError<'rel>)>,
{
    move |i: LocatedSpan<Input, Extra>| match (p.parse(i.clone()), i.group_i9n()) {
        (Ok((i, o)), group_i9n) if rel.partial_cmp(&o.start_i9n, &group_i9n) => Ok((i, o)),
        (Ok((_, o)), group_i9n) => {
            let ie = I9nError::BadI9nRelativeToGroupI9n {
                rel,
                group_i9n,
                actual_i9n: o.start_i9n,
            };
            Err(nom::Err::Error((i, ie).into()))
        }
        (Err(e), _) => Err(e),
    }
}
