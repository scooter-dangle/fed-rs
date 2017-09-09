//! Anonymous, type-based, tagged type unions (here termed 'type federations') in stable Rust
//!
//! # Setup
//! In your crate root, include the crate aliased to `_fed` (since you'll rely on the `init_fed!`
//! macro to declare the `fed` module):
//!
//! ```
//! #[macro_use]
//! extern crate fed as _fed;
//! # fn main() {
//! # }
//! ```
//!
//! Then, define all of the `fed` traits in your crate with this macro call:
//!
//! ```
//! # #[macro_use]
//! # extern crate fed as _fed;
//! init_fed!();
//! # fn main() {
//! # }
//! ```
//!
//! To actually use a type federation, you'll need to implement the type federation traits for all
//! the concrete types you want to comprise the federation, via the `fed!` macro:
//!
//! ```
//! # #[macro_use]
//! # extern crate fed as _fed;
//! # init_fed!();
//! use fed::*;
//!
//! fed!(String, bool, u8, Vec<i32>);
//!
//! fn main() {
//!     let var: Fed4<String, bool, u8, Vec<i32>> = 33.into();
//!     assert!(var.is::<u8>());
//! }
//! ```
//!
//! For the time being, notice that we unfortunately will need to `use` everything in the `fed`
//! module anywhere we want to use type federations. If this crate sees any use where this is an
//! issue, I might work on fixing that. But this is largely intended for prototyping the feature to
//! determine whether a fully fledged, built-in version would be useful for Rust to have.

// // Doesn't work...complains about duplicate implementations.
// // Have to implement non-generically (with concrete types)
//
// impl<A, B> ::std::convert::From<Fed1<A>> for Fed2<A, B> {
//     fn from(val: Fed1<A>) -> Self {
//         match val {
//             Fed1::T1(val) => Fed2::T1(val),
//         }
//     }
// }
//
// impl<A, B> ::std::convert::From<Fed1<B>> for Fed2<A, B> {
//     fn from(val: Fed1<B>) -> Self {
//         match val {
//             Fed1::T1(val) => Fed2::T2(val),
//         }
//     }
// }
//
// // A notion of what a built in type federation concept might look like
// // (instead of the order-dependent `FedX`).
// impl<A | B> ::std::convert::From<(A|)> for (A|B) {
//     fn from(val: (A|)) -> Self {
//         let Ok(val) = val.extract::<A>();
//         val.into()
//     }
// }

#[macro_export]
macro_rules! basic_fed {
    ($typename:ident, @deriving:[$($derive_trait:ident),*], $(($letter:ident, $f_letter:ident) => [$enum_var:ident; $arg_name:ident]),*,) => {
        #[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash, $($derive_trait),*)]
        pub enum $typename< $($letter),* > {
            $(
            $enum_var($letter)
            ),*
        }

        impl< $($letter),* > $typename< $($letter),* > {
            pub fn map_all<T, $($f_letter),*>(self, $( $arg_name : $f_letter ),* ) -> T
            where $(
                $f_letter: FnOnce($letter) -> T
            ),*
            {
                match self { $(
                    $typename::$enum_var(val) => $arg_name(val)
                ),* }
            }

            pub fn as_ref(&self) -> $typename< $(&$letter),* > {
                match self {
                    $(
                    &$typename::$enum_var(ref val) => $typename::$enum_var(val)
                    ),*
                }
            }

            pub fn as_mut(&mut self) -> $typename< $(&mut $letter),* > {
                match self {
                    $(
                    &mut $typename::$enum_var(ref mut val) => $typename::$enum_var(val)
                    ),*
                }
            }
        }
    };
}

/// Setup macro to be called after including the fed crate
///
/// Unfortunately necessary since the type federation properties cannot be implemented generically,
/// and since the client crate needs to own them in order to be able to implement them for external
/// types.
#[macro_export]
macro_rules! init_fed {
    () => { init_fed!(@deriving:[]); };
    (@deriving:[$($derive_trait:ident),*]) => {
        #[allow(dead_code)]
        pub mod fed {
            pub trait Fed: Sized {
                fn is<T>(&self) -> bool
                where Self: Is_<T> {
                    Is_::<T>::is_(self)
                }

                fn extract<T>(self) -> ::std::result::Result<T, <Self as Extract_<T>>::Lower>
                where Self: Extract_<T> {
                    Extract_::<T>::extract_(self)
                }

                fn map_same<T, F>(self, action: F) -> Self
                where F: Fn(T) -> T,
                      Self: MapSame_<T>
                {
                    MapSame_::<T>::map_same_(self, action)
                }
            }

            impl<T: Sized> Fed for T {}

            pub trait Is_<T>: Sized {
                fn is_(&self) -> bool;
            }

            pub trait Extract_<T>: Sized {
                type Lower: Sized;
                fn extract_(self) -> ::std::result::Result<T, Self::Lower>;
            }

            pub trait MapSame_<T>: Sized {
                fn map_same_<F>(self, action: F) -> Self where F: Fn(T) -> T;
            }

            /// A 'never' type
            #[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash, $($derive_trait),*)]
            pub enum Fed0 {}

            #[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash, $($derive_trait),*)]
            pub enum Fed1<A> {
                T1(A),
            }

            impl<A: ::std::default::Default> ::std::default::Default for Fed1<A> {
                fn default() -> Self {
                    A::default().into()
                }
            }

            impl<A> ::std::convert::From<A> for Fed1<A> {
                fn from(t1: A) -> Self {
                    Fed1::T1(t1)
                }
            }

            impl<A> Fed1<A> {
                pub fn map_all<T, F>(self, f_1: F) -> T where F: FnOnce(A) -> T {
                    match self {
                        Fed1::T1(t1) => f_1(t1),
                    }
                }

                pub fn as_ref(&self) -> Fed1<&A> {
                    match self {
                        &Fed1::T1(ref t1) => Fed1::T1(t1),
                    }
                }

                pub fn as_mut(&mut self) -> Fed1<&mut A> {
                    match self {
                        &mut Fed1::T1(ref mut t1) => Fed1::T1(t1),
                    }
                }

                pub fn into_tuple(self) -> (Option<A>,) {
                    match self {
                        Fed1::T1(t1) => (Some(t1),),
                    }
                }
            }

            impl<A: Sized> Is_<A> for Fed1<A> {
                fn is_(&self) -> bool { true }
            }

            impl<'a, A: Sized> Is_<A> for &'a Fed1<A> {
                fn is_(&self) -> bool { true }
            }

            impl<A: Sized> Extract_<A> for Fed1<A> {
                type Lower = Fed0;
                fn extract_(self) -> ::std::result::Result<A, Self::Lower> {
                    match self { Fed1::T1(val) => ::std::result::Result::Ok(val) }
                }
            }

            impl<A: Sized> MapSame_<A> for Fed1<A> {
                fn map_same_<F>(self, action: F) -> Self where F: Fn(A) -> A {
                    match self { Fed1::T1(val) => action(val).into() }
                }
            }

            basic_fed!{
                Fed2,
                @deriving:[$($derive_trait),*],
                (A, FA) => [T1; f_1],
                (B, FB) => [T2; f_2],
            }

            basic_fed!{
                Fed3,
                @deriving:[$($derive_trait),*],
                (A, FA) => [T1; f_1],
                (B, FB) => [T2; f_2],
                (C, FC) => [T3; f_3],
            }

            basic_fed!{
                Fed4,
                @deriving:[$($derive_trait),*],
                (A, FA) => [T1; f_1],
                (B, FB) => [T2; f_2],
                (C, FC) => [T3; f_3],
                (D, FD) => [T4; f_4],
            }

            basic_fed!{
                Fed5,
                @deriving:[$($derive_trait),*],
                (A, FA) => [T1; f_1],
                (B, FB) => [T2; f_2],
                (C, FC) => [T3; f_3],
                (D, FD) => [T4; f_4],
                (E, FE) => [T5; f_5],
            }

            basic_fed!{
                Fed6,
                @deriving:[$($derive_trait),*],
                (A, FA) => [T1; f_1],
                (B, FB) => [T2; f_2],
                (C, FC) => [T3; f_3],
                (D, FD) => [T4; f_4],
                (E, FE) => [T5; f_5],
                (F, FF) => [T6; f_6],
            }

            basic_fed!{
                Fed7,
                @deriving:[$($derive_trait),*],
                (A, FA) => [T1; f_1],
                (B, FB) => [T2; f_2],
                (C, FC) => [T3; f_3],
                (D, FD) => [T4; f_4],
                (E, FE) => [T5; f_5],
                (F, FF) => [T6; f_6],
                (G, FG) => [T7; f_7],
            }

            basic_fed!{
                Fed8,
                @deriving:[$($derive_trait),*],
                (A, FA) => [T1; f_1],
                (B, FB) => [T2; f_2],
                (C, FC) => [T3; f_3],
                (D, FD) => [T4; f_4],
                (E, FE) => [T5; f_5],
                (F, FF) => [T6; f_6],
                (G, FG) => [T7; f_7],
                (H, FH) => [T8; f_8],
            }

            // Don't know how to turn the following `into_tuple` implementations into
            // macros

            impl<A, B> Fed2<A, B> {
                // Hack to facilitate matching in absence of dedicated `match` syntax for
                // type federations
                pub fn into_tuple(self) -> (Option<A>, Option<B>) {
                    match self {
                        Fed2::T1(val) => (Some(val), None),
                        Fed2::T2(val) => (None, Some(val)),
                    }
                }
            }

            impl<A, B, C> Fed3<A, B, C> {
                pub fn into_tuple(self) -> (Option<A>, Option<B>, Option<C>) {
                    match self {
                        Fed3::T1(val) => (Some(val), None, None),
                        Fed3::T2(val) => (None, Some(val), None),
                        Fed3::T3(val) => (None, None, Some(val)),
                    }
                }
            }

            impl<A, B, C, D> Fed4<A, B, C, D> {
                pub fn into_tuple(self) -> (Option<A>, Option<B>, Option<C>, Option<D>) {
                    match self {
                        Fed4::T1(val) => (Some(val), None, None, None),
                        Fed4::T2(val) => (None, Some(val), None, None),
                        Fed4::T3(val) => (None, None, Some(val), None),
                        Fed4::T4(val) => (None, None, None, Some(val)),
                    }
                }
            }

            impl<A, B, C, D, E> Fed5<A, B, C, D, E> {
                pub fn into_tuple(self) -> (Option<A>, Option<B>, Option<C>, Option<D>, Option<E>) {
                    match self {
                        Fed5::T1(val) => (Some(val), None, None, None, None),
                        Fed5::T2(val) => (None, Some(val), None, None, None),
                        Fed5::T3(val) => (None, None, Some(val), None, None),
                        Fed5::T4(val) => (None, None, None, Some(val), None),
                        Fed5::T5(val) => (None, None, None, None, Some(val)),
                    }
                }
            }

            impl<A, B, C, D, E, F> Fed6<A, B, C, D, E, F> {
                pub fn into_tuple(self) -> (Option<A>, Option<B>, Option<C>, Option<D>, Option<E>, Option<F>) {
                    match self {
                        Fed6::T1(val) => (Some(val), None, None, None, None, None),
                        Fed6::T2(val) => (None, Some(val), None, None, None, None),
                        Fed6::T3(val) => (None, None, Some(val), None, None, None),
                        Fed6::T4(val) => (None, None, None, Some(val), None, None),
                        Fed6::T5(val) => (None, None, None, None, Some(val), None),
                        Fed6::T6(val) => (None, None, None, None, None, Some(val)),
                    }
                }
            }

            impl<A, B, C, D, E, F, G> Fed7<A, B, C, D, E, F, G> {
                pub fn into_tuple(self) -> (Option<A>, Option<B>, Option<C>, Option<D>, Option<E>, Option<F>, Option<G>) {
                    match self {
                        Fed7::T1(val) => (Some(val), None, None, None, None, None, None),
                        Fed7::T2(val) => (None, Some(val), None, None, None, None, None),
                        Fed7::T3(val) => (None, None, Some(val), None, None, None, None),
                        Fed7::T4(val) => (None, None, None, Some(val), None, None, None),
                        Fed7::T5(val) => (None, None, None, None, Some(val), None, None),
                        Fed7::T6(val) => (None, None, None, None, None, Some(val), None),
                        Fed7::T7(val) => (None, None, None, None, None, None, Some(val)),
                    }
                }
            }

            impl<A, B, C, D, E, F, G, H> Fed8<A, B, C, D, E, F, G, H> {
                pub fn into_tuple(self) -> (Option<A>, Option<B>, Option<C>, Option<D>, Option<E>, Option<F>, Option<G>, Option<H>) {
                    match self {
                        Fed8::T1(val) => (Some(val), None, None, None, None, None, None, None),
                        Fed8::T2(val) => (None, Some(val), None, None, None, None, None, None),
                        Fed8::T3(val) => (None, None, Some(val), None, None, None, None, None),
                        Fed8::T4(val) => (None, None, None, Some(val), None, None, None, None),
                        Fed8::T5(val) => (None, None, None, None, Some(val), None, None, None),
                        Fed8::T6(val) => (None, None, None, None, None, Some(val), None, None),
                        Fed8::T7(val) => (None, None, None, None, None, None, Some(val), None),
                        Fed8::T8(val) => (None, None, None, None, None, None, None, Some(val)),
                    }
                }
            }
        }
    };
}

#[macro_export]
macro_rules! from_fed {
    ($newtype:ty; $generic:ident; $($component:ty),*; $a:ty, $enum_var:path) => {
        impl ::std::convert::From<$a> for $newtype {
            fn from(val: $a) -> Self {
                $enum_var(val)
            }
        }

        impl<'a> ::std::convert::From<&'a $a> for $generic< $(&'a $component),* > {
            fn from(val: &'a $a) -> Self {
                $enum_var(val)
            }
        }
    };
}

#[macro_export]
macro_rules! fed_traits {
    ($newtype:ty; $generic:ident; $($component:ty),*; $a:ty, $enum_var:path, $lower_type:ident, $($b:ty => $b_enum_var:path),*) => {
        impl Is_<$a> for $newtype {
            fn is_(&self) -> bool {
                match *self {
                    $enum_var(_) => true,
                    _ => false,
                }
            }
        }

        impl<'a> Is_<$a> for &'a $newtype {
            fn is_(&self) -> bool {
                match *self {
                    &$enum_var(_) => true,
                    _ => false,
                }
            }
        }

        impl<'a> Is_<&'a $a> for $generic< $(&'a $component),* > {
            fn is_(&self) -> bool {
                match self {
                    &$enum_var(_) => true,
                    _ => false,
                }
            }
        }

        impl Extract_<$a> for $newtype {
            type Lower = $lower_type< $($b),* >;

            fn extract_(self) -> ::std::result::Result<$a, Self::Lower> {
                match self {
                    $enum_var(val) => ::std::result::Result::Ok(val),

                    $(
                        $b_enum_var(val) => ::std::result::Result::Err(val.into())
                    ),*
                }
            }
        }

        impl<'a> Extract_<&'a $a> for $generic< $(&'a $component),* > {
            type Lower = $lower_type< $(&'a $b),* >;

            fn extract_(self) -> ::std::result::Result<&'a $a, Self::Lower> {
                match self {
                    $enum_var(val) => ::std::result::Result::Ok(val),

                    $(
                        $b_enum_var(val) => ::std::result::Result::Err(val.into())
                    ),*
                }
            }
        }

        impl MapSame_<$a> for $newtype {
            fn map_same_<F>(self, action: F) -> Self where F: Fn($a) -> $a {
                match self {
                    $enum_var(val) => action(val).into(),
                    other @ _ => other,
                }
            }
        }

        impl<'a> MapSame_<&'a $a> for $generic< $(&'a $component),* > {
            fn map_same_<F>(self, action: F) -> Self where F: Fn(&'a $a) -> &'a $a {
                match self {
                    $enum_var(val) => action(val).into(),
                    other @ _ => other,
                }
            }
        }
    };
}

#[macro_export]
macro_rules! fed_promotion {
    ($generic_0:ident => $generic_1:ident; <$($component_0:ty),*> => <$($component_1:ty),*>; $littletype:ty => $bigtype:ty; $($enum_var:path),+) => {
        impl ::std::convert::From<$littletype> for $bigtype {
            fn from(item: $littletype) -> Self {
                match item {
                    $(
                        $enum_var(val) => val.into()
                    ),+
                }
            }
        }

        impl<'a> ::std::convert::From< $generic_0< $(&'a $component_0),* > > for $generic_1< $(&'a $component_1),* > {
            fn from(item: $generic_0< $(&'a $component_0),* >) -> Self {
                match item {
                    $(
                        $enum_var(val) => val.into()
                    ),+
                }
            }
        }
    };
}

/// Macro to implement type federation traits for federations of concrete types
#[macro_export]
macro_rules! fed {
    ($($a:ty),+,) => { fed!( $($a),+ ); };
    ($a:ty) => {
        // Fed1<A> already generically implemented
    };
    ($a:ty, $b:ty)                                           => { fed2!($a, $b);                         };
    ($a:ty, $b:ty, $c:ty)                                    => { fed3!($a, $b, $c);                     };
    ($a:ty, $b:ty, $c:ty, $d:ty)                             => { fed4!($a, $b, $c, $d);                 };
    ($a:ty, $b:ty, $c:ty, $d:ty, $e:ty)                      => { fed5!($a, $b, $c, $d, $e);             };
    ($a:ty, $b:ty, $c:ty, $d:ty, $e:ty, $f:ty)               => { fed6!($a, $b, $c, $d, $e, $f);         };
    ($a:ty, $b:ty, $c:ty, $d:ty, $e:ty, $f:ty, $g:ty)        => { fed7!($a, $b, $c, $d, $e, $f, $g);     };
    ($a:ty, $b:ty, $c:ty, $d:ty, $e:ty, $f:ty, $g:ty, $h:ty) => { fed8!($a, $b, $c, $d, $e, $f, $g, $h); };
}

#[macro_export]
macro_rules! fed2 {
    ($a:ty, $b:ty) => {
        fed2!(Fed2<$a, $b>; $a, $b);
    };
    ($a:ty, $b:ty; @without_children) => {
        fed2!(Fed2<$a, $b>; $a, $b);
    };
    ($newtype:ty; $a:ty, $b:ty) => {
        fed2!($newtype; generic: Fed2; $a, $b; $a, $b);
    };
    ($newtype:ty; generic: $generic:ident; $($component:ty),*; $a:ty, $b:ty) => {
        from_fed!($newtype; $generic; $($component),*; $a, Fed2::T1);
        from_fed!($newtype; $generic; $($component),*; $b, Fed2::T2);

        fed_traits!($newtype; $generic; $($component),*; $a, Fed2::T1, Fed1, $b => Fed2::T2);
        fed_traits!($newtype; $generic; $($component),*; $b, Fed2::T2, Fed1, $a => Fed2::T1);

        fed_promotion!(Fed1 => Fed2; <$a> => <$a, $b>; Fed1<$a> => Fed2<$a, $b>; Fed1::T1);
        fed_promotion!(Fed1 => Fed2; <$b> => <$a, $b>; Fed1<$b> => Fed2<$a, $b>; Fed1::T1);
    };
}

#[macro_export]
macro_rules! fed3 {
    ($a:ty, $b:ty, $c:ty) => {
        fed3!(Fed3<$a, $b, $c>; $a, $b, $c);
    };
    ($a:ty, $b:ty, $c:ty; @without_children) => {
        fed3!(Fed3<$a, $b, $c>; $a, $b, $c; @without_children);
    };
    ($newtype:ty; $a:ty, $b:ty, $c:ty; @without_children) => {
        fed3!(Fed3<$a, $b, $c>; generic: Fed3; $a, $b, $c; $a, $b, $c; @without_children);
    };
    ($newtype:ty; generic: $generic:ident; $($component:ty),*; $a:ty, $b:ty, $c:ty; @without_children) => {
        from_fed!($newtype; $generic; $($component),*; $a, Fed3::T1);
        from_fed!($newtype; $generic; $($component),*; $b, Fed3::T2);
        from_fed!($newtype; $generic; $($component),*; $c, Fed3::T3);

        fed_traits!($newtype; $generic; $($component),*; $c, Fed3::T3, Fed2, $a => Fed3::T1, $b => Fed3::T2                );
        fed_traits!($newtype; $generic; $($component),*; $b, Fed3::T2, Fed2, $a => Fed3::T1,                 $c => Fed3::T3);
        fed_traits!($newtype; $generic; $($component),*; $a, Fed3::T1, Fed2,                 $b => Fed3::T2, $c => Fed3::T3);

        fed_promotion!(Fed1 => Fed3; <$c> => <$a, $b, $c>; Fed1<$c> => Fed3<$a, $b, $c>; Fed1::T1);
        fed_promotion!(Fed1 => Fed3; <$b> => <$a, $b, $c>; Fed1<$b> => Fed3<$a, $b, $c>; Fed1::T1);
        fed_promotion!(Fed1 => Fed3; <$a> => <$a, $b, $c>; Fed1<$a> => Fed3<$a, $b, $c>; Fed1::T1);

        fed_promotion!(Fed2 => Fed3; <$b, $c> => <$a, $b, $c>; Fed2<$b, $c> => Fed3<$a, $b, $c>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed3; <$a, $b> => <$a, $b, $c>; Fed2<$a, $b> => Fed3<$a, $b, $c>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed3; <$a, $c> => <$a, $b, $c>; Fed2<$a, $c> => Fed3<$a, $b, $c>; Fed2::T1, Fed2::T2);

    };
    ($newtype:ty; $a:ty, $b:ty, $c:ty) => {
        fed2!($a, $b;     @without_children);
        fed2!($a,     $c; @without_children);
        fed2!(    $b, $c; @without_children);

        fed3!($a, $b, $c; @without_children);
    };
}

#[macro_export]
macro_rules! fed4 {
    ($a:ty, $b:ty, $c:ty, $d:ty) => {
        fed4!(Fed4<$a, $b, $c, $d>; $a, $b, $c, $d);
    };
    ($a:ty, $b:ty, $c:ty, $d:ty; @without_children) => {
        fed4!(Fed4<$a, $b, $c, $d>; $a, $b, $c, $d; @without_children);
    };
    ($newtype:ty; $a:ty, $b:ty, $c:ty, $d:ty; @without_children) => {
        fed4!(Fed4<$a, $b, $c, $d>; generic: Fed4; $a, $b, $c, $d; $a, $b, $c, $d; @without_children);
    };
    ($newtype:ty; generic: $generic:ident; $($component:ty),*; $a:ty, $b:ty, $c:ty, $d:ty; @without_children) => {
        from_fed!($newtype; $generic; $($component),*; $a, Fed4::T1);
        from_fed!($newtype; $generic; $($component),*; $b, Fed4::T2);
        from_fed!($newtype; $generic; $($component),*; $c, Fed4::T3);
        from_fed!($newtype; $generic; $($component),*; $d, Fed4::T4);

        fed_traits!($newtype; $generic; $($component),*; $d, Fed4::T4, Fed3, $a => Fed4::T1, $b => Fed4::T2, $c => Fed4::T3                );
        fed_traits!($newtype; $generic; $($component),*; $c, Fed4::T3, Fed3, $a => Fed4::T1, $b => Fed4::T2,                 $d => Fed4::T4);
        fed_traits!($newtype; $generic; $($component),*; $b, Fed4::T2, Fed3, $a => Fed4::T1,                 $c => Fed4::T3, $d => Fed4::T4);
        fed_traits!($newtype; $generic; $($component),*; $a, Fed4::T1, Fed3,                 $b => Fed4::T2, $c => Fed4::T3, $d => Fed4::T4);

        fed_promotion!(Fed1 => Fed4; <$d> => <$a, $b, $c, $d>; Fed1<$d> => Fed4<$a, $b, $c, $d>; Fed1::T1);
        fed_promotion!(Fed1 => Fed4; <$c> => <$a, $b, $c, $d>; Fed1<$c> => Fed4<$a, $b, $c, $d>; Fed1::T1);
        fed_promotion!(Fed1 => Fed4; <$b> => <$a, $b, $c, $d>; Fed1<$b> => Fed4<$a, $b, $c, $d>; Fed1::T1);
        fed_promotion!(Fed1 => Fed4; <$a> => <$a, $b, $c, $d>; Fed1<$a> => Fed4<$a, $b, $c, $d>; Fed1::T1);

        fed_promotion!(Fed2 => Fed4; <$c, $d> => <$a, $b, $c, $d>; Fed2<$c, $d> => Fed4<$a, $b, $c, $d>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed4; <$b, $c> => <$a, $b, $c, $d>; Fed2<$b, $c> => Fed4<$a, $b, $c, $d>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed4; <$b, $d> => <$a, $b, $c, $d>; Fed2<$b, $d> => Fed4<$a, $b, $c, $d>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed4; <$a, $b> => <$a, $b, $c, $d>; Fed2<$a, $b> => Fed4<$a, $b, $c, $d>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed4; <$a, $c> => <$a, $b, $c, $d>; Fed2<$a, $c> => Fed4<$a, $b, $c, $d>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed4; <$a, $d> => <$a, $b, $c, $d>; Fed2<$a, $d> => Fed4<$a, $b, $c, $d>; Fed2::T1, Fed2::T2);

        fed_promotion!(Fed3 => Fed4; <$b, $c, $d> => <$a, $b, $c, $d>; Fed3<$b, $c, $d> => Fed4<$a, $b, $c, $d>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed4; <$a, $b, $c> => <$a, $b, $c, $d>; Fed3<$a, $b, $c> => Fed4<$a, $b, $c, $d>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed4; <$a, $b, $d> => <$a, $b, $c, $d>; Fed3<$a, $b, $d> => Fed4<$a, $b, $c, $d>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed4; <$a, $c, $d> => <$a, $b, $c, $d>; Fed3<$a, $c, $d> => Fed4<$a, $b, $c, $d>; Fed3::T1, Fed3::T2, Fed3::T3);

    };
    ($newtype:ty; $a:ty, $b:ty, $c:ty, $d:ty) => {
        fed3!($a, $b, $c;     @without_children);
        fed3!($a, $b,     $d; @without_children);
        fed3!($a,     $c, $d; @without_children);
        fed3!(    $b, $c, $d; @without_children);

        fed2!($a, $b;         @without_children);
        fed2!($a,     $c;     @without_children);
        fed2!($a,         $d; @without_children);
        fed2!(    $b, $c;     @without_children);
        fed2!(    $b,     $d; @without_children);
        fed2!(        $c, $d; @without_children);

        fed4!($newtype; $a, $b, $c, $d; @without_children);
    };
}

#[macro_export]
macro_rules! fed5 {
    ($a:ty, $b:ty, $c:ty, $d:ty, $e:ty) => {
        fed5!(Fed5<$a, $b, $c, $d, $e>; $a, $b, $c, $d, $e);
    };
    ($a:ty, $b:ty, $c:ty, $d:ty, $e:ty; @without_children) => {
        fed5!(Fed5<$a, $b, $c, $d, $e>; $a, $b, $c, $d, $e; @without_children);
    };
    ($newtype:ty; $a:ty, $b:ty, $c:ty, $d:ty, $e:ty; @without_children) => {
        fed5!(Fed5<$a, $b, $c, $d, $e>; generic: Fed5; $a, $b, $c, $d, $e; $a, $b, $c, $d, $e; @without_children);
    };
    ($newtype:ty; generic: $generic:ident; $($component:ty),*; $a:ty, $b:ty, $c:ty, $d:ty, $e:ty; @without_children) => {
        from_fed!($newtype; $generic; $($component),*; $a, Fed5::T1);
        from_fed!($newtype; $generic; $($component),*; $b, Fed5::T2);
        from_fed!($newtype; $generic; $($component),*; $c, Fed5::T3);
        from_fed!($newtype; $generic; $($component),*; $d, Fed5::T4);
        from_fed!($newtype; $generic; $($component),*; $e, Fed5::T5);

        fed_traits!($newtype; $generic; $($component),*; $e, Fed5::T5, Fed4, $a => Fed5::T1, $b => Fed5::T2, $c => Fed5::T3, $d => Fed5::T4                );
        fed_traits!($newtype; $generic; $($component),*; $d, Fed5::T4, Fed4, $a => Fed5::T1, $b => Fed5::T2, $c => Fed5::T3,                 $e => Fed5::T5);
        fed_traits!($newtype; $generic; $($component),*; $c, Fed5::T3, Fed4, $a => Fed5::T1, $b => Fed5::T2,                 $d => Fed5::T4, $e => Fed5::T5);
        fed_traits!($newtype; $generic; $($component),*; $b, Fed5::T2, Fed4, $a => Fed5::T1,                 $c => Fed5::T3, $d => Fed5::T4, $e => Fed5::T5);
        fed_traits!($newtype; $generic; $($component),*; $a, Fed5::T1, Fed4,                 $b => Fed5::T2, $c => Fed5::T3, $d => Fed5::T4, $e => Fed5::T5);

        fed_promotion!(Fed1 => Fed5; <$e> => <$a, $b, $c, $d, $e>; Fed1<$e> => Fed5<$a, $b, $c, $d, $e>; Fed1::T1);
        fed_promotion!(Fed1 => Fed5; <$d> => <$a, $b, $c, $d, $e>; Fed1<$d> => Fed5<$a, $b, $c, $d, $e>; Fed1::T1);
        fed_promotion!(Fed1 => Fed5; <$c> => <$a, $b, $c, $d, $e>; Fed1<$c> => Fed5<$a, $b, $c, $d, $e>; Fed1::T1);
        fed_promotion!(Fed1 => Fed5; <$b> => <$a, $b, $c, $d, $e>; Fed1<$b> => Fed5<$a, $b, $c, $d, $e>; Fed1::T1);
        fed_promotion!(Fed1 => Fed5; <$a> => <$a, $b, $c, $d, $e>; Fed1<$a> => Fed5<$a, $b, $c, $d, $e>; Fed1::T1);

        fed_promotion!(Fed2 => Fed5; <$d, $e> => <$a, $b, $c, $d, $e>; Fed2<$d, $e> => Fed5<$a, $b, $c, $d, $e>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed5; <$c, $d> => <$a, $b, $c, $d, $e>; Fed2<$c, $d> => Fed5<$a, $b, $c, $d, $e>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed5; <$c, $e> => <$a, $b, $c, $d, $e>; Fed2<$c, $e> => Fed5<$a, $b, $c, $d, $e>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed5; <$b, $c> => <$a, $b, $c, $d, $e>; Fed2<$b, $c> => Fed5<$a, $b, $c, $d, $e>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed5; <$b, $d> => <$a, $b, $c, $d, $e>; Fed2<$b, $d> => Fed5<$a, $b, $c, $d, $e>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed5; <$b, $e> => <$a, $b, $c, $d, $e>; Fed2<$b, $e> => Fed5<$a, $b, $c, $d, $e>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed5; <$a, $b> => <$a, $b, $c, $d, $e>; Fed2<$a, $b> => Fed5<$a, $b, $c, $d, $e>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed5; <$a, $c> => <$a, $b, $c, $d, $e>; Fed2<$a, $c> => Fed5<$a, $b, $c, $d, $e>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed5; <$a, $d> => <$a, $b, $c, $d, $e>; Fed2<$a, $d> => Fed5<$a, $b, $c, $d, $e>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed5; <$a, $e> => <$a, $b, $c, $d, $e>; Fed2<$a, $e> => Fed5<$a, $b, $c, $d, $e>; Fed2::T1, Fed2::T2);

        fed_promotion!(Fed3 => Fed5; <$c, $d, $e> => <$a, $b, $c, $d, $e>; Fed3<$c, $d, $e> => Fed5<$a, $b, $c, $d, $e>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed5; <$b, $c, $d> => <$a, $b, $c, $d, $e>; Fed3<$b, $c, $d> => Fed5<$a, $b, $c, $d, $e>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed5; <$b, $c, $e> => <$a, $b, $c, $d, $e>; Fed3<$b, $c, $e> => Fed5<$a, $b, $c, $d, $e>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed5; <$b, $d, $e> => <$a, $b, $c, $d, $e>; Fed3<$b, $d, $e> => Fed5<$a, $b, $c, $d, $e>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed5; <$a, $b, $c> => <$a, $b, $c, $d, $e>; Fed3<$a, $b, $c> => Fed5<$a, $b, $c, $d, $e>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed5; <$a, $b, $d> => <$a, $b, $c, $d, $e>; Fed3<$a, $b, $d> => Fed5<$a, $b, $c, $d, $e>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed5; <$a, $b, $e> => <$a, $b, $c, $d, $e>; Fed3<$a, $b, $e> => Fed5<$a, $b, $c, $d, $e>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed5; <$a, $c, $d> => <$a, $b, $c, $d, $e>; Fed3<$a, $c, $d> => Fed5<$a, $b, $c, $d, $e>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed5; <$a, $c, $e> => <$a, $b, $c, $d, $e>; Fed3<$a, $c, $e> => Fed5<$a, $b, $c, $d, $e>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed5; <$a, $d, $e> => <$a, $b, $c, $d, $e>; Fed3<$a, $d, $e> => Fed5<$a, $b, $c, $d, $e>; Fed3::T1, Fed3::T2, Fed3::T3);

        fed_promotion!(Fed4 => Fed5; <$b, $c, $d, $e> => <$a, $b, $c, $d, $e>; Fed4<$b, $c, $d, $e> => Fed5<$a, $b, $c, $d, $e>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed5; <$a, $b, $c, $d> => <$a, $b, $c, $d, $e>; Fed4<$a, $b, $c, $d> => Fed5<$a, $b, $c, $d, $e>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed5; <$a, $b, $c, $e> => <$a, $b, $c, $d, $e>; Fed4<$a, $b, $c, $e> => Fed5<$a, $b, $c, $d, $e>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed5; <$a, $b, $d, $e> => <$a, $b, $c, $d, $e>; Fed4<$a, $b, $d, $e> => Fed5<$a, $b, $c, $d, $e>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed5; <$a, $c, $d, $e> => <$a, $b, $c, $d, $e>; Fed4<$a, $c, $d, $e> => Fed5<$a, $b, $c, $d, $e>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);

    };
    ($newtype:ty; $a:ty, $b:ty, $c:ty, $d:ty, $e:ty) => {
        fed4!($a, $b, $c, $d;     @without_children);
        fed4!($a, $b, $c,     $e; @without_children);
        fed4!($a, $b,     $d, $e; @without_children);
        fed4!($a,     $c, $d, $e; @without_children);
        fed4!(    $b, $c, $d, $e; @without_children);

        fed3!($a, $b, $c;         @without_children);
        fed3!($a, $b,     $d;     @without_children);
        fed3!($a, $b,         $e; @without_children);
        fed3!($a,     $c, $d;     @without_children);
        fed3!($a,     $c,     $e; @without_children);
        fed3!($a, $d,         $e; @without_children);
        fed3!(    $b, $c, $d;     @without_children);
        fed3!(    $b, $c,     $e; @without_children);
        fed3!(    $b,     $d, $e; @without_children);
        fed3!(        $c, $d, $e; @without_children);

        fed2!($a, $b;             @without_children);
        fed2!($a,     $c;         @without_children);
        fed2!($a,         $d;     @without_children);
        fed2!($a,             $e; @without_children);
        fed2!(    $b, $c;         @without_children);
        fed2!(    $b,     $d;     @without_children);
        fed2!(    $b,         $e; @without_children);
        fed2!(        $c, $d;     @without_children);
        fed2!(        $c,     $e; @without_children);
        fed2!(            $d, $e; @without_children);

        fed5!($newtype; $a, $b, $c, $d, $e; @without_children);
    };
}

#[macro_export]
macro_rules! fed6 {
    ($a:ty, $b:ty, $c:ty, $d:ty, $e:ty, $f:ty) => {
        fed6!(Fed6<$a, $b, $c, $d, $e, $f>; $a, $b, $c, $d, $e, $f);
    };
    ($a:ty, $b:ty, $c:ty, $d:ty, $e:ty, $f:ty; @without_children) => {
        fed6!(Fed6<$a, $b, $c, $d, $e, $f>; $a, $b, $c, $d, $e, $f; @without_children);
    };
    ($newtype:ty; $a:ty, $b:ty, $c:ty, $d:ty, $e:ty, $f:ty; @without_children) => {
        fed6!(Fed6<$a, $b, $c, $d, $e, $f>; generic: Fed6; $a, $b, $c, $d, $e, $f; $a, $b, $c, $d, $e, $f; @without_children);
    };
    ($newtype:ty; generic: $generic:ident; $($component:ty),*; $a:ty, $b:ty, $c:ty, $d:ty, $e:ty, $f:ty; @without_children) => {
        from_fed!($newtype; $generic; $($component),*; $a, Fed6::T1);
        from_fed!($newtype; $generic; $($component),*; $b, Fed6::T2);
        from_fed!($newtype; $generic; $($component),*; $c, Fed6::T3);
        from_fed!($newtype; $generic; $($component),*; $d, Fed6::T4);
        from_fed!($newtype; $generic; $($component),*; $e, Fed6::T5);
        from_fed!($newtype; $generic; $($component),*; $f, Fed6::T6);

        fed_traits!($newtype; $generic; $($component),*; $f, Fed6::T6, Fed5, $a => Fed6::T1, $b => Fed6::T2, $c => Fed6::T3, $d => Fed6::T4, $e => Fed6::T5                );
        fed_traits!($newtype; $generic; $($component),*; $e, Fed6::T5, Fed5, $a => Fed6::T1, $b => Fed6::T2, $c => Fed6::T3, $d => Fed6::T4,                 $f => Fed6::T6);
        fed_traits!($newtype; $generic; $($component),*; $d, Fed6::T4, Fed5, $a => Fed6::T1, $b => Fed6::T2, $c => Fed6::T3,                 $e => Fed6::T5, $f => Fed6::T6);
        fed_traits!($newtype; $generic; $($component),*; $c, Fed6::T3, Fed5, $a => Fed6::T1, $b => Fed6::T2,                 $d => Fed6::T4, $e => Fed6::T5, $f => Fed6::T6);
        fed_traits!($newtype; $generic; $($component),*; $b, Fed6::T2, Fed5, $a => Fed6::T1,                 $c => Fed6::T3, $d => Fed6::T4, $e => Fed6::T5, $f => Fed6::T6);
        fed_traits!($newtype; $generic; $($component),*; $a, Fed6::T1, Fed5,                 $b => Fed6::T2, $c => Fed6::T3, $d => Fed6::T4, $e => Fed6::T5, $f => Fed6::T6);

        fed_promotion!(Fed1 => Fed6; <$f> => <$a, $b, $c, $d, $e, $f>; Fed1<$f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed1::T1);
        fed_promotion!(Fed1 => Fed6; <$e> => <$a, $b, $c, $d, $e, $f>; Fed1<$e> => Fed6<$a, $b, $c, $d, $e, $f>; Fed1::T1);
        fed_promotion!(Fed1 => Fed6; <$d> => <$a, $b, $c, $d, $e, $f>; Fed1<$d> => Fed6<$a, $b, $c, $d, $e, $f>; Fed1::T1);
        fed_promotion!(Fed1 => Fed6; <$c> => <$a, $b, $c, $d, $e, $f>; Fed1<$c> => Fed6<$a, $b, $c, $d, $e, $f>; Fed1::T1);
        fed_promotion!(Fed1 => Fed6; <$b> => <$a, $b, $c, $d, $e, $f>; Fed1<$b> => Fed6<$a, $b, $c, $d, $e, $f>; Fed1::T1);
        fed_promotion!(Fed1 => Fed6; <$a> => <$a, $b, $c, $d, $e, $f>; Fed1<$a> => Fed6<$a, $b, $c, $d, $e, $f>; Fed1::T1);

        fed_promotion!(Fed2 => Fed6; <$e, $f> => <$a, $b, $c, $d, $e, $f>; Fed2<$e, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed6; <$d, $e> => <$a, $b, $c, $d, $e, $f>; Fed2<$d, $e> => Fed6<$a, $b, $c, $d, $e, $f>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed6; <$d, $f> => <$a, $b, $c, $d, $e, $f>; Fed2<$d, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed6; <$c, $d> => <$a, $b, $c, $d, $e, $f>; Fed2<$c, $d> => Fed6<$a, $b, $c, $d, $e, $f>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed6; <$c, $e> => <$a, $b, $c, $d, $e, $f>; Fed2<$c, $e> => Fed6<$a, $b, $c, $d, $e, $f>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed6; <$c, $f> => <$a, $b, $c, $d, $e, $f>; Fed2<$c, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed6; <$b, $c> => <$a, $b, $c, $d, $e, $f>; Fed2<$b, $c> => Fed6<$a, $b, $c, $d, $e, $f>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed6; <$b, $d> => <$a, $b, $c, $d, $e, $f>; Fed2<$b, $d> => Fed6<$a, $b, $c, $d, $e, $f>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed6; <$b, $e> => <$a, $b, $c, $d, $e, $f>; Fed2<$b, $e> => Fed6<$a, $b, $c, $d, $e, $f>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed6; <$b, $f> => <$a, $b, $c, $d, $e, $f>; Fed2<$b, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed6; <$a, $b> => <$a, $b, $c, $d, $e, $f>; Fed2<$a, $b> => Fed6<$a, $b, $c, $d, $e, $f>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed6; <$a, $c> => <$a, $b, $c, $d, $e, $f>; Fed2<$a, $c> => Fed6<$a, $b, $c, $d, $e, $f>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed6; <$a, $d> => <$a, $b, $c, $d, $e, $f>; Fed2<$a, $d> => Fed6<$a, $b, $c, $d, $e, $f>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed6; <$a, $e> => <$a, $b, $c, $d, $e, $f>; Fed2<$a, $e> => Fed6<$a, $b, $c, $d, $e, $f>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed6; <$a, $f> => <$a, $b, $c, $d, $e, $f>; Fed2<$a, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed2::T1, Fed2::T2);

        fed_promotion!(Fed3 => Fed6; <$d, $e, $f> => <$a, $b, $c, $d, $e, $f>; Fed3<$d, $e, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed6; <$c, $d, $e> => <$a, $b, $c, $d, $e, $f>; Fed3<$c, $d, $e> => Fed6<$a, $b, $c, $d, $e, $f>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed6; <$c, $d, $f> => <$a, $b, $c, $d, $e, $f>; Fed3<$c, $d, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed6; <$c, $e, $f> => <$a, $b, $c, $d, $e, $f>; Fed3<$c, $e, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed6; <$b, $c, $d> => <$a, $b, $c, $d, $e, $f>; Fed3<$b, $c, $d> => Fed6<$a, $b, $c, $d, $e, $f>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed6; <$b, $c, $e> => <$a, $b, $c, $d, $e, $f>; Fed3<$b, $c, $e> => Fed6<$a, $b, $c, $d, $e, $f>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed6; <$b, $c, $f> => <$a, $b, $c, $d, $e, $f>; Fed3<$b, $c, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed6; <$b, $d, $e> => <$a, $b, $c, $d, $e, $f>; Fed3<$b, $d, $e> => Fed6<$a, $b, $c, $d, $e, $f>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed6; <$b, $d, $f> => <$a, $b, $c, $d, $e, $f>; Fed3<$b, $d, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed6; <$b, $e, $f> => <$a, $b, $c, $d, $e, $f>; Fed3<$b, $e, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed6; <$a, $b, $c> => <$a, $b, $c, $d, $e, $f>; Fed3<$a, $b, $c> => Fed6<$a, $b, $c, $d, $e, $f>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed6; <$a, $b, $d> => <$a, $b, $c, $d, $e, $f>; Fed3<$a, $b, $d> => Fed6<$a, $b, $c, $d, $e, $f>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed6; <$a, $b, $e> => <$a, $b, $c, $d, $e, $f>; Fed3<$a, $b, $e> => Fed6<$a, $b, $c, $d, $e, $f>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed6; <$a, $b, $f> => <$a, $b, $c, $d, $e, $f>; Fed3<$a, $b, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed6; <$a, $c, $d> => <$a, $b, $c, $d, $e, $f>; Fed3<$a, $c, $d> => Fed6<$a, $b, $c, $d, $e, $f>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed6; <$a, $c, $e> => <$a, $b, $c, $d, $e, $f>; Fed3<$a, $c, $e> => Fed6<$a, $b, $c, $d, $e, $f>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed6; <$a, $c, $f> => <$a, $b, $c, $d, $e, $f>; Fed3<$a, $c, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed6; <$a, $d, $e> => <$a, $b, $c, $d, $e, $f>; Fed3<$a, $d, $e> => Fed6<$a, $b, $c, $d, $e, $f>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed6; <$a, $d, $f> => <$a, $b, $c, $d, $e, $f>; Fed3<$a, $d, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed6; <$a, $e, $f> => <$a, $b, $c, $d, $e, $f>; Fed3<$a, $e, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed3::T1, Fed3::T2, Fed3::T3);

        fed_promotion!(Fed4 => Fed6; <$c, $d, $e, $f> => <$a, $b, $c, $d, $e, $f>; Fed4<$c, $d, $e, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed6; <$b, $c, $d, $e> => <$a, $b, $c, $d, $e, $f>; Fed4<$b, $c, $d, $e> => Fed6<$a, $b, $c, $d, $e, $f>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed6; <$b, $c, $d, $f> => <$a, $b, $c, $d, $e, $f>; Fed4<$b, $c, $d, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed6; <$b, $c, $e, $f> => <$a, $b, $c, $d, $e, $f>; Fed4<$b, $c, $e, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed6; <$b, $d, $e, $f> => <$a, $b, $c, $d, $e, $f>; Fed4<$b, $d, $e, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed6; <$a, $b, $c, $d> => <$a, $b, $c, $d, $e, $f>; Fed4<$a, $b, $c, $d> => Fed6<$a, $b, $c, $d, $e, $f>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed6; <$a, $b, $c, $e> => <$a, $b, $c, $d, $e, $f>; Fed4<$a, $b, $c, $e> => Fed6<$a, $b, $c, $d, $e, $f>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed6; <$a, $b, $c, $f> => <$a, $b, $c, $d, $e, $f>; Fed4<$a, $b, $c, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed6; <$a, $b, $d, $e> => <$a, $b, $c, $d, $e, $f>; Fed4<$a, $b, $d, $e> => Fed6<$a, $b, $c, $d, $e, $f>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed6; <$a, $b, $d, $f> => <$a, $b, $c, $d, $e, $f>; Fed4<$a, $b, $d, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed6; <$a, $b, $e, $f> => <$a, $b, $c, $d, $e, $f>; Fed4<$a, $b, $e, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed6; <$a, $c, $d, $e> => <$a, $b, $c, $d, $e, $f>; Fed4<$a, $c, $d, $e> => Fed6<$a, $b, $c, $d, $e, $f>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed6; <$a, $c, $d, $f> => <$a, $b, $c, $d, $e, $f>; Fed4<$a, $c, $d, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed6; <$a, $c, $e, $f> => <$a, $b, $c, $d, $e, $f>; Fed4<$a, $c, $e, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed6; <$a, $d, $e, $f> => <$a, $b, $c, $d, $e, $f>; Fed4<$a, $d, $e, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);

        fed_promotion!(Fed5 => Fed6; <$b, $c, $d, $e, $f> => <$a, $b, $c, $d, $e, $f>; Fed5<$b, $c, $d, $e, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed6; <$a, $b, $c, $d, $e> => <$a, $b, $c, $d, $e, $f>; Fed5<$a, $b, $c, $d, $e> => Fed6<$a, $b, $c, $d, $e, $f>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed6; <$a, $b, $c, $d, $f> => <$a, $b, $c, $d, $e, $f>; Fed5<$a, $b, $c, $d, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed6; <$a, $b, $c, $e, $f> => <$a, $b, $c, $d, $e, $f>; Fed5<$a, $b, $c, $e, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed6; <$a, $b, $d, $e, $f> => <$a, $b, $c, $d, $e, $f>; Fed5<$a, $b, $d, $e, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed6; <$a, $c, $d, $e, $f> => <$a, $b, $c, $d, $e, $f>; Fed5<$a, $c, $d, $e, $f> => Fed6<$a, $b, $c, $d, $e, $f>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);

    };
    ($newtype:ty; $a:ty, $b:ty, $c:ty, $d:ty, $e:ty, $f:ty) => {
        fed5!($a, $b, $c, $d, $e;     @without_children);
        fed5!($a, $b, $c, $d,     $f; @without_children);
        fed5!($a, $b, $c,     $e, $f; @without_children);
        fed5!($a, $b,     $d, $e, $f; @without_children);
        fed5!($a,     $c, $d, $e, $f; @without_children);
        fed5!(    $b, $c, $d, $e, $f; @without_children);

        fed4!($a, $b, $c, $d;         @without_children);
        fed4!($a, $b, $c,     $e;     @without_children);
        fed4!($a, $b, $c,         $f; @without_children);
        fed4!($a, $b,     $d, $e;     @without_children);
        fed4!($a, $b,     $d,     $f; @without_children);
        fed4!($a, $b,         $e, $f; @without_children);
        fed4!($a,     $c, $d, $e;     @without_children);
        fed4!($a,     $c, $d,     $f; @without_children);
        fed4!($a,     $c,     $e, $f; @without_children);
        fed4!($a,         $d, $e, $f; @without_children);
        fed4!(    $b, $c, $d, $e;     @without_children);
        fed4!(    $b, $c, $d,     $f; @without_children);
        fed4!(    $b, $c,     $e, $f; @without_children);
        fed4!(    $b,     $d, $e, $f; @without_children);
        fed4!(        $c, $d, $e, $f; @without_children);

        fed3!($a, $b, $c;             @without_children);
        fed3!($a, $b,     $d;         @without_children);
        fed3!($a, $b,         $e;     @without_children);
        fed3!($a,     $c, $d;         @without_children);
        fed3!($a,     $c,     $e;     @without_children);
        fed3!($a,         $d, $e;     @without_children);
        fed3!(    $b, $c, $d;         @without_children);
        fed3!(    $b, $c,     $e;     @without_children);
        fed3!(    $b,     $d, $e;     @without_children);
        fed3!(        $c, $d, $e;     @without_children);
        fed3!($a, $b,             $f; @without_children);
        fed3!($a,     $c,         $f; @without_children);
        fed3!($a,         $d,     $f; @without_children);
        fed3!($a,             $e, $f; @without_children);
        fed3!(    $b, $c,         $f; @without_children);
        fed3!(    $b,     $d,     $f; @without_children);
        fed3!(    $b,         $e, $f; @without_children);
        fed3!(        $c, $d,     $f; @without_children);
        fed3!(        $c,     $e, $f; @without_children);
        fed3!(            $d, $e, $f; @without_children);

        fed2!($a, $b;                 @without_children);
        fed2!($a,     $c;             @without_children);
        fed2!($a,         $d;         @without_children);
        fed2!($a,             $e;     @without_children);
        fed2!($a,                 $f; @without_children);
        fed2!(    $b, $c;             @without_children);
        fed2!(    $b,     $d;         @without_children);
        fed2!(    $b,         $e;     @without_children);
        fed2!(    $b,             $f; @without_children);
        fed2!(        $c, $d;         @without_children);
        fed2!(        $c,     $e;     @without_children);
        fed2!(        $c,         $f; @without_children);
        fed2!(            $d, $e;     @without_children);
        fed2!(            $d,     $f; @without_children);
        fed2!(                $e, $f; @without_children);

        fed6!($newtype; $a, $b, $c, $d, $e, $f; @without_children);
    };
}

#[macro_export]
macro_rules! fed7 {
    ($a:ty, $b:ty, $c:ty, $d:ty, $e:ty, $f:ty, $g:ty) => {
        fed7!(Fed7<$a, $b, $c, $d, $e, $f, $g>; $a, $b, $c, $d, $e, $f, $g);
    };
    ($a:ty, $b:ty, $c:ty, $d:ty, $e:ty, $f:ty, $g:ty; @without_children) => {
        fed7!(Fed7<$a, $b, $c, $d, $e, $f, $g>; $a, $b, $c, $d, $e, $f, $g; @without_children);
    };
    ($newtype:ty; $a:ty, $b:ty, $c:ty, $d:ty, $e:ty, $f:ty, $g:ty; @without_children) => {
        fed7!(Fed7<$a, $b, $c, $d, $e, $f, $g>; generic: Fed7; $a, $b, $c, $d, $e, $f, $g; $a, $b, $c, $d, $e, $f, $g; @without_children);
    };
    ($newtype:ty; generic: $generic:ident; $($component:ty),*; $a:ty, $b:ty, $c:ty, $d:ty, $e:ty, $f:ty, $g:ty; @without_children) => {
        from_fed!($newtype; $generic; $($component),*; $a, Fed7::T1);
        from_fed!($newtype; $generic; $($component),*; $b, Fed7::T2);
        from_fed!($newtype; $generic; $($component),*; $c, Fed7::T3);
        from_fed!($newtype; $generic; $($component),*; $d, Fed7::T4);
        from_fed!($newtype; $generic; $($component),*; $e, Fed7::T5);
        from_fed!($newtype; $generic; $($component),*; $f, Fed7::T6);
        from_fed!($newtype; $generic; $($component),*; $g, Fed7::T7);

        fed_traits!($newtype; $generic; $($component),*; $g, Fed7::T7, Fed6, $a => Fed7::T1, $b => Fed7::T2, $c => Fed7::T3, $d => Fed7::T4, $e => Fed7::T5, $f => Fed7::T6                );
        fed_traits!($newtype; $generic; $($component),*; $f, Fed7::T6, Fed6, $a => Fed7::T1, $b => Fed7::T2, $c => Fed7::T3, $d => Fed7::T4, $e => Fed7::T5,                 $g => Fed7::T7);
        fed_traits!($newtype; $generic; $($component),*; $e, Fed7::T5, Fed6, $a => Fed7::T1, $b => Fed7::T2, $c => Fed7::T3, $d => Fed7::T4,                 $f => Fed7::T6, $g => Fed7::T7);
        fed_traits!($newtype; $generic; $($component),*; $d, Fed7::T4, Fed6, $a => Fed7::T1, $b => Fed7::T2, $c => Fed7::T3,                 $e => Fed7::T5, $f => Fed7::T6, $g => Fed7::T7);
        fed_traits!($newtype; $generic; $($component),*; $c, Fed7::T3, Fed6, $a => Fed7::T1, $b => Fed7::T2,                 $d => Fed7::T4, $e => Fed7::T5, $f => Fed7::T6, $g => Fed7::T7);
        fed_traits!($newtype; $generic; $($component),*; $b, Fed7::T2, Fed6, $a => Fed7::T1,                 $c => Fed7::T3, $d => Fed7::T4, $e => Fed7::T5, $f => Fed7::T6, $g => Fed7::T7);
        fed_traits!($newtype; $generic; $($component),*; $a, Fed7::T1, Fed6,                 $b => Fed7::T2, $c => Fed7::T3, $d => Fed7::T4, $e => Fed7::T5, $f => Fed7::T6, $g => Fed7::T7);

        fed_promotion!(Fed1 => Fed7; <$g> => <$a, $b, $c, $d, $e, $f, $g>; Fed1<$g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed1::T1);
        fed_promotion!(Fed1 => Fed7; <$f> => <$a, $b, $c, $d, $e, $f, $g>; Fed1<$f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed1::T1);
        fed_promotion!(Fed1 => Fed7; <$e> => <$a, $b, $c, $d, $e, $f, $g>; Fed1<$e> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed1::T1);
        fed_promotion!(Fed1 => Fed7; <$d> => <$a, $b, $c, $d, $e, $f, $g>; Fed1<$d> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed1::T1);
        fed_promotion!(Fed1 => Fed7; <$c> => <$a, $b, $c, $d, $e, $f, $g>; Fed1<$c> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed1::T1);
        fed_promotion!(Fed1 => Fed7; <$b> => <$a, $b, $c, $d, $e, $f, $g>; Fed1<$b> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed1::T1);
        fed_promotion!(Fed1 => Fed7; <$a> => <$a, $b, $c, $d, $e, $f, $g>; Fed1<$a> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed1::T1);

        fed_promotion!(Fed2 => Fed7; <$f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed2<$f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed7; <$e, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed2<$e, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed7; <$e, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed2<$e, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed7; <$d, $e> => <$a, $b, $c, $d, $e, $f, $g>; Fed2<$d, $e> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed7; <$d, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed2<$d, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed7; <$d, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed2<$d, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed7; <$c, $d> => <$a, $b, $c, $d, $e, $f, $g>; Fed2<$c, $d> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed7; <$c, $e> => <$a, $b, $c, $d, $e, $f, $g>; Fed2<$c, $e> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed7; <$c, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed2<$c, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed7; <$c, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed2<$c, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed7; <$b, $c> => <$a, $b, $c, $d, $e, $f, $g>; Fed2<$b, $c> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed7; <$b, $d> => <$a, $b, $c, $d, $e, $f, $g>; Fed2<$b, $d> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed7; <$b, $e> => <$a, $b, $c, $d, $e, $f, $g>; Fed2<$b, $e> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed7; <$b, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed2<$b, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed7; <$b, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed2<$b, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed7; <$a, $b> => <$a, $b, $c, $d, $e, $f, $g>; Fed2<$a, $b> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed7; <$a, $c> => <$a, $b, $c, $d, $e, $f, $g>; Fed2<$a, $c> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed7; <$a, $d> => <$a, $b, $c, $d, $e, $f, $g>; Fed2<$a, $d> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed7; <$a, $e> => <$a, $b, $c, $d, $e, $f, $g>; Fed2<$a, $e> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed7; <$a, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed2<$a, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed7; <$a, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed2<$a, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed2::T1, Fed2::T2);

        fed_promotion!(Fed3 => Fed7; <$e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$e, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$d, $e, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$d, $e, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$d, $e, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$d, $e, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$d, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$d, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$c, $d, $e> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$c, $d, $e> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$c, $d, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$c, $d, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$c, $d, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$c, $d, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$c, $e, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$c, $e, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$c, $e, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$c, $e, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$c, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$c, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$b, $c, $d> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$b, $c, $d> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$b, $c, $e> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$b, $c, $e> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$b, $c, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$b, $c, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$b, $c, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$b, $c, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$b, $d, $e> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$b, $d, $e> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$b, $d, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$b, $d, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$b, $d, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$b, $d, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$b, $e, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$b, $e, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$b, $e, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$b, $e, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$b, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$b, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$a, $b, $c> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$a, $b, $c> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$a, $b, $d> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$a, $b, $d> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$a, $b, $e> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$a, $b, $e> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$a, $b, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$a, $b, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$a, $b, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$a, $b, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$a, $c, $d> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$a, $c, $d> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$a, $c, $e> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$a, $c, $e> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$a, $c, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$a, $c, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$a, $c, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$a, $c, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$a, $d, $e> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$a, $d, $e> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$a, $d, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$a, $d, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$a, $d, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$a, $d, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$a, $e, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$a, $e, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$a, $e, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$a, $e, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed7; <$a, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed3<$a, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed3::T1, Fed3::T2, Fed3::T3);


        fed_promotion!(Fed4 => Fed7; <$d, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$d, $e, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$c, $d, $e, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$c, $d, $e, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$c, $d, $e, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$c, $d, $e, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$c, $d, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$c, $d, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$c, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$c, $e, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$b, $c, $d, $e> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$b, $c, $d, $e> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$b, $c, $d, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$b, $c, $d, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$b, $c, $e, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$b, $c, $e, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$b, $c, $e, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$b, $c, $e, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$b, $c, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$b, $c, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$b, $d, $e, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$b, $d, $e, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$b, $d, $e, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$b, $d, $e, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$b, $d, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$b, $d, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$b, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$b, $e, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$a, $b, $c, $d> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$a, $b, $c, $d> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$a, $b, $c, $e> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$a, $b, $c, $e> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$a, $b, $d, $e> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$a, $b, $d, $e> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$a, $b, $d, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$a, $b, $d, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$a, $b, $e, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$a, $b, $e, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$a, $b, $e, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$a, $b, $e, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$a, $b, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$a, $b, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$a, $c, $d, $e> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$a, $c, $d, $e> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$a, $c, $d, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$a, $c, $d, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$a, $c, $e, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$a, $c, $e, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$a, $c, $e, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$a, $c, $e, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$a, $c, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$a, $c, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$a, $d, $e, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$a, $d, $e, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$a, $d, $e, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$a, $d, $e, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$a, $d, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$a, $d, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed7; <$a, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed4<$a, $e, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);


        fed_promotion!(Fed5 => Fed7; <$c, $d, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed5<$c, $d, $e, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed7; <$b, $c, $d, $e, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed5<$b, $c, $d, $e, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed7; <$b, $c, $d, $e, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed5<$b, $c, $d, $e, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed7; <$b, $c, $d, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed5<$b, $c, $d, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed7; <$b, $c, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed5<$b, $c, $e, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed7; <$b, $d, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed5<$b, $d, $e, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed7; <$a, $b, $c, $d, $e> => <$a, $b, $c, $d, $e, $f, $g>; Fed5<$a, $b, $c, $d, $e> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed7; <$a, $b, $c, $d, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed5<$a, $b, $c, $d, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed7; <$a, $b, $c, $e, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed5<$a, $b, $c, $e, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed7; <$a, $b, $c, $e, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed5<$a, $b, $c, $e, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed7; <$a, $b, $d, $e, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed5<$a, $b, $d, $e, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed7; <$a, $b, $d, $e, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed5<$a, $b, $d, $e, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed7; <$a, $b, $d, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed5<$a, $b, $d, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed7; <$a, $b, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed5<$a, $b, $e, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed7; <$a, $c, $d, $e, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed5<$a, $c, $d, $e, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed7; <$a, $c, $d, $e, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed5<$a, $c, $d, $e, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed7; <$a, $c, $d, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed5<$a, $c, $d, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed7; <$a, $c, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed5<$a, $c, $e, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed7; <$a, $d, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed5<$a, $d, $e, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);


        fed_promotion!(Fed6 => Fed7; <$b, $c, $d, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed6<$b, $c, $d, $e, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed7; <$a, $b, $c, $d, $e, $f> => <$a, $b, $c, $d, $e, $f, $g>; Fed6<$a, $b, $c, $d, $e, $f> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed7; <$a, $b, $c, $d, $e, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed6<$a, $b, $c, $d, $e, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed7; <$a, $b, $c, $d, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed6<$a, $b, $c, $d, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed7; <$a, $b, $c, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed6<$a, $b, $c, $e, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed7; <$a, $b, $d, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed6<$a, $b, $d, $e, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed7; <$a, $c, $d, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g>; Fed6<$a, $c, $d, $e, $f, $g> => Fed7<$a, $b, $c, $d, $e, $f, $g>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);

    };
    ($newtype:ty; $a:ty, $b:ty, $c:ty, $d:ty, $e:ty, $f:ty, $g:ty) => {
        fed6!($a, $b, $c, $d, $e, $f;     @without_children);
        fed6!($a, $b, $c, $d, $e,     $g; @without_children);
        fed6!($a, $b, $c, $d,     $f, $g; @without_children);
        fed6!($a, $b, $c,     $e, $f, $g; @without_children);
        fed6!($a, $b,     $d, $e, $f, $g; @without_children);
        fed6!($a,     $c, $d, $e, $f, $g; @without_children);
        fed6!(    $b, $c, $d, $e, $f, $g; @without_children);

        fed5!($a, $b, $c, $d, $e;         @without_children);
        fed5!($a, $b, $c, $d,     $f;     @without_children);
        fed5!($a, $b, $c,     $e, $f;     @without_children);
        fed5!($a, $b,     $d, $e, $f;     @without_children);
        fed5!($a,     $c, $d, $e, $f;     @without_children);
        fed5!(    $b, $c, $d, $e, $f;     @without_children);
        //
        fed5!($a, $b, $c, $d,         $g; @without_children);
        fed5!($a, $b, $c,     $e,     $g; @without_children);
        fed5!($a, $b, $c,         $f, $g; @without_children);
        fed5!($a, $b,     $d, $e,     $g; @without_children);
        fed5!($a, $b,     $d,     $f, $g; @without_children);
        fed5!($a, $b,         $e, $f, $g; @without_children);
        fed5!($a,     $c, $d, $e,     $g; @without_children);
        fed5!($a,     $c, $d,     $f, $g; @without_children);
        fed5!($a,     $c,     $e, $f, $g; @without_children);
        fed5!($a,         $d, $e, $f, $g; @without_children);
        fed5!(    $b, $c, $d, $e,     $g; @without_children);
        fed5!(    $b, $c, $d,     $f, $g; @without_children);
        fed5!(    $b, $c,     $e, $f, $g; @without_children);
        fed5!(    $b,     $d, $e, $f, $g; @without_children);
        fed5!(        $c, $d, $e, $f, $g; @without_children);

        fed4!($a, $b, $c, $d;             @without_children);
        fed4!($a, $b, $c,     $e;         @without_children);
        fed4!($a, $b, $c,         $f;     @without_children);
        fed4!($a, $b,     $d, $e;         @without_children);
        fed4!($a, $b,     $d,     $f;     @without_children);
        fed4!($a, $b,         $e, $f;     @without_children);
        fed4!($a,     $c, $d, $e;         @without_children);
        fed4!($a,     $c, $d,     $f;     @without_children);
        fed4!($a,     $c,     $e, $f;     @without_children);
        fed4!($a,         $d, $e, $f;     @without_children);
        fed4!(    $b, $c, $d, $e;         @without_children);
        fed4!(    $b, $c, $d,     $f;     @without_children);
        fed4!(    $b, $c,     $e, $f;     @without_children);
        fed4!(    $b,     $d, $e, $f;     @without_children);
        fed4!(        $c, $d, $e, $f;     @without_children);
        //
        fed4!($a, $b, $c,             $g; @without_children);
        fed4!($a, $b,     $d,         $g; @without_children);
        fed4!($a, $b,         $e,     $g; @without_children);
        fed4!($a,     $c, $d,         $g; @without_children);
        fed4!($a,     $c,     $e,     $g; @without_children);
        fed4!($a,         $d, $e,     $g; @without_children);
        fed4!(    $b, $c, $d,         $g; @without_children);
        fed4!(    $b, $c,     $e,     $g; @without_children);
        fed4!(    $b,     $d, $e,     $g; @without_children);
        fed4!(        $c, $d, $e,     $g; @without_children);
        fed4!($a, $b,             $f, $g; @without_children);
        fed4!($a,     $c,         $f, $g; @without_children);
        fed4!($a,         $d,     $f, $g; @without_children);
        fed4!($a,             $e, $f, $g; @without_children);
        fed4!(    $b, $c,         $f, $g; @without_children);
        fed4!(    $b,     $d,     $f, $g; @without_children);
        fed4!(    $b,         $e, $f, $g; @without_children);
        fed4!(        $c, $d,     $f, $g; @without_children);
        fed4!(        $c,     $e, $f, $g; @without_children);
        fed4!(            $d, $e, $f, $g; @without_children);

        fed3!($a, $b, $c;                 @without_children);
        fed3!($a, $b,     $d;             @without_children);
        fed3!($a, $b,         $e;         @without_children);
        fed3!($a,     $c, $d;             @without_children);
        fed3!($a,     $c,     $e;         @without_children);
        fed3!($a,         $d, $e;         @without_children);
        fed3!(    $b, $c, $d;             @without_children);
        fed3!(    $b, $c,     $e;         @without_children);
        fed3!(    $b,     $d, $e;         @without_children);
        fed3!(        $c, $d, $e;         @without_children);
        fed3!($a, $b,             $f;     @without_children);
        fed3!($a,     $c,         $f;     @without_children);
        fed3!($a,         $d,     $f;     @without_children);
        fed3!($a,             $e, $f;     @without_children);
        fed3!(    $b, $c,         $f;     @without_children);
        fed3!(    $b,     $d,     $f;     @without_children);
        fed3!(    $b,         $e, $f;     @without_children);
        fed3!(        $c, $d,     $f;     @without_children);
        fed3!(        $c,     $e, $f;     @without_children);
        fed3!(            $d, $e, $f;     @without_children);
        //
        fed3!($a, $b,                 $g; @without_children);
        fed3!($a,     $c,             $g; @without_children);
        fed3!($a,         $d,         $g; @without_children);
        fed3!($a,             $e,     $g; @without_children);
        fed3!($a,                 $f, $g; @without_children);
        fed3!(    $b, $c,             $g; @without_children);
        fed3!(    $b,     $d,         $g; @without_children);
        fed3!(    $b,         $e,     $g; @without_children);
        fed3!(    $b,             $f, $g; @without_children);
        fed3!(        $c, $d,         $g; @without_children);
        fed3!(        $c,     $e,     $g; @without_children);
        fed3!(        $c,         $f, $g; @without_children);
        fed3!(            $d, $e,     $g; @without_children);
        fed3!(            $d,     $f, $g; @without_children);
        fed3!(                $e, $f, $g; @without_children);

        fed2!($a, $b;                     @without_children);
        fed2!($a,     $c;                 @without_children);
        fed2!($a,         $d;             @without_children);
        fed2!($a,             $e;         @without_children);
        fed2!($a,                 $f;     @without_children);
        fed2!($a,                     $g; @without_children);
        fed2!(    $b, $c;                 @without_children);
        fed2!(    $b,     $d;             @without_children);
        fed2!(    $b,         $e;         @without_children);
        fed2!(    $b,             $f;     @without_children);
        fed2!(    $b,                 $g; @without_children);
        fed2!(        $c, $d;             @without_children);
        fed2!(        $c,     $e;         @without_children);
        fed2!(        $c,         $f;     @without_children);
        fed2!(        $c,             $g; @without_children);
        fed2!(            $d, $e;         @without_children);
        fed2!(            $d,     $f;     @without_children);
        fed2!(            $d,         $g; @without_children);
        fed2!(                $e, $f;     @without_children);
        fed2!(                $e,     $g; @without_children);
        fed2!(                    $f, $g; @without_children);

        fed7!($newtype; $a, $b, $c, $d, $e, $f, $g; @without_children);
    };
}

#[macro_export]
macro_rules! fed8 {
    ($a:ty, $b:ty, $c:ty, $d:ty, $e:ty, $f:ty, $g:ty, $h:ty) => {
        fed8!(Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; $a, $b, $c, $d, $e, $f, $g, $h);
    };
    ($a:ty, $b:ty, $c:ty, $d:ty, $e:ty, $f:ty, $g:ty, $h:ty; @without_children) => {
        fed8!(Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; $a, $b, $c, $d, $e, $f, $g, $h; @without_children);
    };
    ($newtype:ty; $a:ty, $b:ty, $c:ty, $d:ty, $e:ty, $f:ty, $g:ty, $h:ty; @without_children) => {
        fed8!(Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; generic: Fed8; $a, $b, $c, $d, $e, $f, $g, $h; $a, $b, $c, $d, $e, $f, $g, $h; @without_children);
    };
    ($newtype:ty; generic: $generic:ident; $($component:ty),*; $a:ty, $b:ty, $c:ty, $d:ty, $e:ty, $f:ty, $g:ty, $h:ty; @without_children) => {
        from_fed!($newtype; $generic; $($component),*; $a, Fed8::T1);
        from_fed!($newtype; $generic; $($component),*; $b, Fed8::T2);
        from_fed!($newtype; $generic; $($component),*; $c, Fed8::T3);
        from_fed!($newtype; $generic; $($component),*; $d, Fed8::T4);
        from_fed!($newtype; $generic; $($component),*; $e, Fed8::T5);
        from_fed!($newtype; $generic; $($component),*; $f, Fed8::T6);
        from_fed!($newtype; $generic; $($component),*; $g, Fed8::T7);
        from_fed!($newtype; $generic; $($component),*; $h, Fed8::T8);

        fed_traits!($newtype; $generic; $($component),*; $h, Fed8::T8, Fed7, $a => Fed8::T1, $b => Fed8::T2, $c => Fed8::T3, $d => Fed8::T4, $e => Fed8::T5, $f => Fed8::T6, $g => Fed8::T7                );
        fed_traits!($newtype; $generic; $($component),*; $g, Fed8::T7, Fed7, $a => Fed8::T1, $b => Fed8::T2, $c => Fed8::T3, $d => Fed8::T4, $e => Fed8::T5, $f => Fed8::T6,                 $h => Fed8::T8);
        fed_traits!($newtype; $generic; $($component),*; $f, Fed8::T6, Fed7, $a => Fed8::T1, $b => Fed8::T2, $c => Fed8::T3, $d => Fed8::T4, $e => Fed8::T5,                 $g => Fed8::T7, $h => Fed8::T8);
        fed_traits!($newtype; $generic; $($component),*; $e, Fed8::T5, Fed7, $a => Fed8::T1, $b => Fed8::T2, $c => Fed8::T3, $d => Fed8::T4,                 $f => Fed8::T6, $g => Fed8::T7, $h => Fed8::T8);
        fed_traits!($newtype; $generic; $($component),*; $d, Fed8::T4, Fed7, $a => Fed8::T1, $b => Fed8::T2, $c => Fed8::T3,                 $e => Fed8::T5, $f => Fed8::T6, $g => Fed8::T7, $h => Fed8::T8);
        fed_traits!($newtype; $generic; $($component),*; $c, Fed8::T3, Fed7, $a => Fed8::T1, $b => Fed8::T2,                 $d => Fed8::T4, $e => Fed8::T5, $f => Fed8::T6, $g => Fed8::T7, $h => Fed8::T8);
        fed_traits!($newtype; $generic; $($component),*; $b, Fed8::T2, Fed7, $a => Fed8::T1,                 $c => Fed8::T3, $d => Fed8::T4, $e => Fed8::T5, $f => Fed8::T6, $g => Fed8::T7, $h => Fed8::T8);
        fed_traits!($newtype; $generic; $($component),*; $a, Fed8::T1, Fed7,                 $b => Fed8::T2, $c => Fed8::T3, $d => Fed8::T4, $e => Fed8::T5, $f => Fed8::T6, $g => Fed8::T7, $h => Fed8::T8);

        fed_promotion!(Fed1 => Fed8; <$h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed1<$h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed1::T1);
        fed_promotion!(Fed1 => Fed8; <$g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed1<$g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed1::T1);
        fed_promotion!(Fed1 => Fed8; <$f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed1<$f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed1::T1);
        fed_promotion!(Fed1 => Fed8; <$e> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed1<$e> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed1::T1);
        fed_promotion!(Fed1 => Fed8; <$d> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed1<$d> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed1::T1);
        fed_promotion!(Fed1 => Fed8; <$c> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed1<$c> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed1::T1);
        fed_promotion!(Fed1 => Fed8; <$b> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed1<$b> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed1::T1);
        fed_promotion!(Fed1 => Fed8; <$a> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed1<$a> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed1::T1);


        fed_promotion!(Fed2 => Fed8; <$g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed2<$g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed8; <$f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed2<$f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed8; <$f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed2<$f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed8; <$e, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed2<$e, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed8; <$e, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed2<$e, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed8; <$e, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed2<$e, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed8; <$d, $e> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed2<$d, $e> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed8; <$d, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed2<$d, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed8; <$d, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed2<$d, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed8; <$d, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed2<$d, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed8; <$c, $d> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed2<$c, $d> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed8; <$c, $e> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed2<$c, $e> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed8; <$c, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed2<$c, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed8; <$c, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed2<$c, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed8; <$c, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed2<$c, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed8; <$b, $c> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed2<$b, $c> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed8; <$b, $d> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed2<$b, $d> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed8; <$b, $e> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed2<$b, $e> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed8; <$b, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed2<$b, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed8; <$b, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed2<$b, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed8; <$b, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed2<$b, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed8; <$a, $b> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed2<$a, $b> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed2::T1, Fed2::T2);
        fed_promotion!(Fed2 => Fed8; <$a, $c> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed2<$a, $c> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed2::T1, Fed2::T2);

        fed_promotion!(Fed3 => Fed8; <$f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$e, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$e, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$e, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$e, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$e, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$d, $e, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$d, $e, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$d, $e, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$d, $e, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$d, $e, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$d, $e, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$d, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$d, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$d, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$d, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$d, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$d, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$c, $d, $e> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$c, $d, $e> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$c, $d, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$c, $d, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$c, $d, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$c, $d, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$c, $d, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$c, $d, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$c, $e, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$c, $e, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$c, $e, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$c, $e, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$c, $e, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$c, $e, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$c, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$c, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$c, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$c, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$c, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$c, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$b, $c, $d> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$b, $c, $d> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$b, $c, $e> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$b, $c, $e> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$b, $c, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$b, $c, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$b, $c, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$b, $c, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$b, $c, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$b, $c, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$b, $d, $e> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$b, $d, $e> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$b, $d, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$b, $d, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$b, $d, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$b, $d, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$b, $d, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$b, $d, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$b, $e, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$b, $e, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$b, $e, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$b, $e, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$b, $e, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$b, $e, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$b, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$b, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$b, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$b, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$b, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$b, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$a, $b, $c> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$a, $b, $c> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$a, $b, $d> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$a, $b, $d> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$a, $b, $e> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$a, $b, $e> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$a, $b, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$a, $b, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$a, $b, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$a, $b, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$a, $b, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$a, $b, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$a, $c, $d> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$a, $c, $d> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);
        fed_promotion!(Fed3 => Fed8; <$a, $c, $e> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed3<$a, $c, $e> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed3::T1, Fed3::T2, Fed3::T3);

        fed_promotion!(Fed4 => Fed8; <$e, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$e, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$d, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$d, $e, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$d, $e, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$d, $e, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$d, $e, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$d, $e, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$d, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$d, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$c, $d, $e, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$c, $d, $e, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$c, $d, $e, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$c, $d, $e, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$c, $d, $e, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$c, $d, $e, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$c, $d, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$c, $d, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$c, $d, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$c, $d, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$c, $d, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$c, $d, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$c, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$c, $e, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$c, $e, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$c, $e, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$c, $e, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$c, $e, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$c, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$c, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$b, $c, $d, $e> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$b, $c, $d, $e> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$b, $c, $d, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$b, $c, $d, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$b, $c, $d, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$b, $c, $d, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$b, $c, $d, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$b, $c, $d, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$b, $c, $e, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$b, $c, $e, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$b, $c, $e, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$b, $c, $e, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$b, $c, $e, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$b, $c, $e, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$b, $c, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$b, $c, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$b, $c, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$b, $c, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$b, $c, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$b, $c, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$b, $d, $e, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$b, $d, $e, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$b, $d, $e, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$b, $d, $e, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$b, $d, $e, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$b, $d, $e, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$b, $d, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$b, $d, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$b, $d, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$b, $d, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$b, $d, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$b, $d, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$b, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$b, $e, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$b, $e, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$b, $e, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$b, $e, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$b, $e, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$b, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$b, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$a, $b, $c, $d> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$a, $b, $c, $d> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$a, $b, $c, $e> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$a, $b, $c, $e> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$a, $b, $c, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$a, $b, $c, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$a, $b, $c, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$a, $b, $c, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$a, $b, $c, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$a, $b, $c, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$a, $b, $d, $e> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$a, $b, $d, $e> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$a, $b, $d, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$a, $b, $d, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$a, $b, $d, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$a, $b, $d, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$a, $b, $d, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$a, $b, $d, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$a, $b, $e, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$a, $b, $e, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$a, $b, $e, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$a, $b, $e, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$a, $b, $e, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$a, $b, $e, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$a, $b, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$a, $b, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$a, $b, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$a, $b, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$a, $b, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$a, $b, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$a, $c, $d, $e> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$a, $c, $d, $e> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$a, $c, $d, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$a, $c, $d, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$a, $c, $d, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$a, $c, $d, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$a, $c, $d, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$a, $c, $d, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$a, $c, $e, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$a, $c, $e, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);
        fed_promotion!(Fed4 => Fed8; <$a, $c, $e, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed4<$a, $c, $e, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed4::T1, Fed4::T2, Fed4::T3, Fed4::T4);

        fed_promotion!(Fed5 => Fed8; <$d, $e, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$d, $e, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$c, $d, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$c, $d, $e, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$c, $d, $e, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$c, $d, $e, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$c, $d, $e, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$c, $d, $e, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$c, $d, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$c, $d, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$c, $e, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$c, $e, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$b, $c, $d, $e, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$b, $c, $d, $e, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$b, $c, $d, $e, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$b, $c, $d, $e, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$b, $c, $d, $e, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$b, $c, $d, $e, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$b, $c, $d, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$b, $c, $d, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$b, $c, $d, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$b, $c, $d, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$b, $c, $d, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$b, $c, $d, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$b, $c, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$b, $c, $e, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$b, $c, $e, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$b, $c, $e, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$b, $c, $e, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$b, $c, $e, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$b, $c, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$b, $c, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$b, $d, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$b, $d, $e, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$b, $d, $e, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$b, $d, $e, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$b, $d, $e, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$b, $d, $e, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$b, $d, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$b, $d, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$b, $e, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$b, $e, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $b, $c, $d, $e> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $b, $c, $d, $e> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $b, $c, $d, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $b, $c, $d, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $b, $c, $d, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $b, $c, $d, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $b, $c, $d, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $b, $c, $d, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $b, $c, $e, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $b, $c, $e, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $b, $c, $e, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $b, $c, $e, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $b, $c, $e, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $b, $c, $e, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $b, $c, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $b, $c, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $b, $c, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $b, $c, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $b, $c, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $b, $c, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $b, $d, $e, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $b, $d, $e, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $b, $d, $e, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $b, $d, $e, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $b, $d, $e, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $b, $d, $e, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $b, $d, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $b, $d, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $b, $d, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $b, $d, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $b, $d, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $b, $d, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $b, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $b, $e, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $b, $e, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $b, $e, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $b, $e, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $b, $e, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $b, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $b, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $c, $d, $e, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $c, $d, $e, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $c, $d, $e, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $c, $d, $e, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $c, $d, $e, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $c, $d, $e, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $c, $d, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $c, $d, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $c, $d, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $c, $d, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $c, $d, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $c, $d, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $c, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $c, $e, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $c, $e, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $c, $e, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);
        fed_promotion!(Fed5 => Fed8; <$a, $c, $e, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed5<$a, $c, $e, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed5::T1, Fed5::T2, Fed5::T3, Fed5::T4, Fed5::T5);

        fed_promotion!(Fed6 => Fed8; <$c, $d, $e, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$c, $d, $e, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$b, $c, $d, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$b, $c, $d, $e, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$b, $c, $d, $e, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$b, $c, $d, $e, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$b, $c, $d, $e, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$b, $c, $d, $e, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$b, $c, $d, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$b, $c, $d, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$b, $c, $e, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$b, $c, $e, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$b, $d, $e, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$b, $d, $e, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$a, $b, $c, $d, $e, $f> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$a, $b, $c, $d, $e, $f> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$a, $b, $c, $d, $e, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$a, $b, $c, $d, $e, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$a, $b, $c, $d, $e, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$a, $b, $c, $d, $e, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$a, $b, $c, $d, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$a, $b, $c, $d, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$a, $b, $c, $d, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$a, $b, $c, $d, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$a, $b, $c, $d, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$a, $b, $c, $d, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$a, $b, $c, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$a, $b, $c, $e, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$a, $b, $c, $e, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$a, $b, $c, $e, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$a, $b, $c, $e, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$a, $b, $c, $e, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$a, $b, $c, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$a, $b, $c, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$a, $b, $d, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$a, $b, $d, $e, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$a, $b, $d, $e, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$a, $b, $d, $e, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$a, $b, $d, $e, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$a, $b, $d, $e, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$a, $b, $d, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$a, $b, $d, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$a, $b, $e, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$a, $b, $e, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$a, $c, $d, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$a, $c, $d, $e, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$a, $c, $d, $e, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$a, $c, $d, $e, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$a, $c, $d, $e, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$a, $c, $d, $e, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$a, $c, $d, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$a, $c, $d, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);
        fed_promotion!(Fed6 => Fed8; <$a, $c, $e, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed6<$a, $c, $e, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed6::T1, Fed6::T2, Fed6::T3, Fed6::T4, Fed6::T5, Fed6::T6);

        fed_promotion!(Fed7 => Fed8; <$b, $c, $d, $e, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed7<$b, $c, $d, $e, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed7::T1, Fed7::T2, Fed7::T3, Fed7::T4, Fed7::T5, Fed7::T6, Fed7::T7);
        fed_promotion!(Fed7 => Fed8; <$a, $b, $c, $d, $e, $f, $g> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed7<$a, $b, $c, $d, $e, $f, $g> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed7::T1, Fed7::T2, Fed7::T3, Fed7::T4, Fed7::T5, Fed7::T6, Fed7::T7);
        fed_promotion!(Fed7 => Fed8; <$a, $b, $c, $d, $e, $f, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed7<$a, $b, $c, $d, $e, $f, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed7::T1, Fed7::T2, Fed7::T3, Fed7::T4, Fed7::T5, Fed7::T6, Fed7::T7);
        fed_promotion!(Fed7 => Fed8; <$a, $b, $c, $d, $e, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed7<$a, $b, $c, $d, $e, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed7::T1, Fed7::T2, Fed7::T3, Fed7::T4, Fed7::T5, Fed7::T6, Fed7::T7);
        fed_promotion!(Fed7 => Fed8; <$a, $b, $c, $d, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed7<$a, $b, $c, $d, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed7::T1, Fed7::T2, Fed7::T3, Fed7::T4, Fed7::T5, Fed7::T6, Fed7::T7);
        fed_promotion!(Fed7 => Fed8; <$a, $b, $c, $e, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed7<$a, $b, $c, $e, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed7::T1, Fed7::T2, Fed7::T3, Fed7::T4, Fed7::T5, Fed7::T6, Fed7::T7);
        fed_promotion!(Fed7 => Fed8; <$a, $b, $d, $e, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed7<$a, $b, $d, $e, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed7::T1, Fed7::T2, Fed7::T3, Fed7::T4, Fed7::T5, Fed7::T6, Fed7::T7);
        fed_promotion!(Fed7 => Fed8; <$a, $c, $d, $e, $f, $g, $h> => <$a, $b, $c, $d, $e, $f, $g, $h>; Fed7<$a, $c, $d, $e, $f, $g, $h> => Fed8<$a, $b, $c, $d, $e, $f, $g, $h>; Fed7::T1, Fed7::T2, Fed7::T3, Fed7::T4, Fed7::T5, Fed7::T6, Fed7::T7);

    };
    ($newtype:ty; $a:ty, $b:ty, $c:ty, $d:ty, $e:ty, $f:ty, $g:ty, $h:ty) => {
        fed7!($a, $b, $c, $d, $e, $f, $g;     @without_children);
        fed7!($a, $b, $c, $d, $e, $f,     $h; @without_children);
        fed7!($a, $b, $c, $d, $e,     $g, $h; @without_children);
        fed7!($a, $b, $c, $d,     $f, $g, $h; @without_children);
        fed7!($a, $b, $c,     $e, $f, $g, $h; @without_children);
        fed7!($a, $b,     $d, $e, $f, $g, $h; @without_children);
        fed7!($a,     $c, $d, $e, $f, $g, $h; @without_children);
        fed7!(    $b, $c, $d, $e, $f, $g, $h; @without_children);

        fed6!($a, $b, $c, $d, $e, $f;         @without_children);
        fed6!($a, $b, $c, $d, $e,     $g;     @without_children);
        fed6!($a, $b, $c, $d,     $f, $g;     @without_children);
        fed6!($a, $b, $c,     $e, $f, $g;     @without_children);
        fed6!($a, $b,     $d, $e, $f, $g;     @without_children);
        fed6!($a,     $c, $d, $e, $f, $g;     @without_children);
        fed6!(    $b, $c, $d, $e, $f, $g;     @without_children);
        fed6!($a, $b, $c, $d, $e,         $h; @without_children);
        fed6!($a, $b, $c, $d,     $f,     $h; @without_children);
        fed6!($a, $b, $c,     $e, $f,     $h; @without_children);
        fed6!($a, $b,     $d, $e, $f,     $h; @without_children);
        fed6!($a,     $c, $d, $e, $f,     $h; @without_children);
        fed6!(    $b, $c, $d, $e, $f,     $h; @without_children);
        fed6!($a, $b, $c, $d,         $g, $h; @without_children);
        fed6!($a, $b, $c,     $e,     $g, $h; @without_children);
        fed6!($a, $b, $c,         $f, $g, $h; @without_children);
        fed6!($a, $b,     $d, $e,     $g, $h; @without_children);
        fed6!($a, $b,     $d,     $f, $g, $h; @without_children);
        fed6!($a, $b,         $e, $f, $g, $h; @without_children);
        fed6!($a,     $c, $d, $e,     $g, $h; @without_children);
        fed6!($a,     $c, $d,     $f, $g, $h; @without_children);
        fed6!($a,     $c,     $e, $f, $g, $h; @without_children);
        fed6!($a,         $d, $e, $f, $g, $h; @without_children);
        fed6!(    $b, $c, $d, $e,     $g, $h; @without_children);
        fed6!(    $b, $c, $d,     $f, $g, $h; @without_children);
        fed6!(    $b, $c,     $e, $f, $g, $h; @without_children);
        fed6!(    $b,     $d, $e, $f, $g, $h; @without_children);
        fed6!(        $c, $d, $e, $f, $g, $h; @without_children);

        fed5!($a, $b, $c, $d, $e;             @without_children);
        fed5!($a, $b, $c, $d,     $f;         @without_children);
        fed5!($a, $b, $c,     $e, $f;         @without_children);
        fed5!($a, $b,     $d, $e, $f;         @without_children);
        fed5!($a,     $c, $d, $e, $f;         @without_children);
        fed5!(    $b, $c, $d, $e, $f;         @without_children);
        fed5!($a, $b, $c, $d,         $g;     @without_children);
        fed5!($a, $b, $c,     $e,     $g;     @without_children);
        fed5!($a, $b, $c,         $f, $g;     @without_children);
        fed5!($a, $b,     $d, $e,     $g;     @without_children);
        fed5!($a, $b,     $d,     $f, $g;     @without_children);
        fed5!($a, $b,         $e, $f, $g;     @without_children);
        fed5!($a,     $c, $d, $e,     $g;     @without_children);
        fed5!($a,     $c, $d,     $f, $g;     @without_children);
        fed5!($a,     $c,     $e, $f, $g;     @without_children);
        fed5!($a,         $d, $e, $f, $g;     @without_children);
        fed5!(    $b, $c, $d, $e,     $g;     @without_children);
        fed5!(    $b, $c, $d,     $f, $g;     @without_children);
        fed5!(    $b, $c,     $e, $f, $g;     @without_children);
        fed5!(    $b,     $d, $e, $f, $g;     @without_children);
        fed5!(        $c, $d, $e, $f, $g;     @without_children);
        fed5!($a, $b, $c, $d,             $h; @without_children);
        fed5!($a, $b, $c,     $e,         $h; @without_children);
        fed5!($a, $b, $c,         $f,     $h; @without_children);
        fed5!($a, $b,     $d, $e,         $h; @without_children);
        fed5!($a, $b,     $d,     $f,     $h; @without_children);
        fed5!($a, $b,         $e, $f,     $h; @without_children);
        fed5!($a,     $c, $d, $e,         $h; @without_children);
        fed5!($a,     $c, $d,     $f,     $h; @without_children);
        fed5!($a,     $c,     $e, $f,     $h; @without_children);
        fed5!($a,         $d, $e, $f,     $h; @without_children);
        fed5!(    $b, $c, $d, $e,         $h; @without_children);
        fed5!(    $b, $c, $d,     $f,     $h; @without_children);
        fed5!(    $b, $c,     $e, $f,     $h; @without_children);
        fed5!(    $b,     $d, $e, $f,     $h; @without_children);
        fed5!(        $c, $d, $e, $f,     $h; @without_children);
        fed5!($a, $b, $c,             $g, $h; @without_children);
        fed5!($a, $b,     $d,         $g, $h; @without_children);
        fed5!($a, $b,         $e,     $g, $h; @without_children);
        fed5!($a,     $c, $d,         $g, $h; @without_children);
        fed5!($a,     $c,     $e,     $g, $h; @without_children);
        fed5!($a,         $d, $e,     $g, $h; @without_children);
        fed5!(    $b, $c, $d,         $g, $h; @without_children);
        fed5!(    $b, $c,     $e,     $g, $h; @without_children);
        fed5!(    $b,     $d, $e,     $g, $h; @without_children);
        fed5!(        $c, $d, $e,     $g, $h; @without_children);
        fed5!($a, $b,             $f, $g, $h; @without_children);
        fed5!($a,     $c,         $f, $g, $h; @without_children);
        fed5!($a,         $d,     $f, $g, $h; @without_children);
        fed5!($a,             $e, $f, $g, $h; @without_children);
        fed5!(    $b, $c,         $f, $g, $h; @without_children);
        fed5!(    $b,     $d,     $f, $g, $h; @without_children);
        fed5!(    $b,         $e, $f, $g, $h; @without_children);
        fed5!(        $c, $d,     $f, $g, $h; @without_children);
        fed5!(        $c,     $e, $f, $g, $h; @without_children);
        fed5!(            $d, $e, $f, $g, $h; @without_children);

        fed4!($a, $b, $c, $d;                 @without_children);
        fed4!($a, $b, $c,     $e;             @without_children);
        fed4!($a, $b, $c,         $f;         @without_children);
        fed4!($a, $b,     $d, $e;             @without_children);
        fed4!($a, $b,     $d,     $f;         @without_children);
        fed4!($a, $b,         $e, $f;         @without_children);
        fed4!($a,     $c, $d, $e;             @without_children);
        fed4!($a,     $c, $d,     $f;         @without_children);
        fed4!($a,     $c,     $e, $f;         @without_children);
        fed4!($a,         $d, $e, $f;         @without_children);
        fed4!(    $b, $c, $d, $e;             @without_children);
        fed4!(    $b, $c, $d,     $f;         @without_children);
        fed4!(    $b, $c,     $e, $f;         @without_children);
        fed4!(    $b,     $d, $e, $f;         @without_children);
        fed4!(        $c, $d, $e, $f;         @without_children);
        fed4!($a, $b, $c,             $g;     @without_children);
        fed4!($a, $b,     $d,         $g;     @without_children);
        fed4!($a, $b,         $e,     $g;     @without_children);
        fed4!($a,     $c, $d,         $g;     @without_children);
        fed4!($a,     $c,     $e,     $g;     @without_children);
        fed4!($a,         $d, $e,     $g;     @without_children);
        fed4!(    $b, $c, $d,         $g;     @without_children);
        fed4!(    $b, $c,     $e,     $g;     @without_children);
        fed4!(    $b,     $d, $e,     $g;     @without_children);
        fed4!(        $c, $d, $e,     $g;     @without_children);
        fed4!($a, $b,             $f, $g;     @without_children);
        fed4!($a,     $c,         $f, $g;     @without_children);
        fed4!($a,         $d,     $f, $g;     @without_children);
        fed4!($a,             $e, $f, $g;     @without_children);
        fed4!(    $b, $c,         $f, $g;     @without_children);
        fed4!(    $b,     $d,     $f, $g;     @without_children);
        fed4!(    $b,         $e, $f, $g;     @without_children);
        fed4!(        $c, $d,     $f, $g;     @without_children);
        fed4!(        $c,     $e, $f, $g;     @without_children);
        fed4!(            $d, $e, $f, $g;     @without_children);
        fed4!($a, $b, $c,                 $h; @without_children);
        fed4!($a, $b,     $d,             $h; @without_children);
        fed4!($a, $b,         $e,         $h; @without_children);
        fed4!($a,     $c, $d,             $h; @without_children);
        fed4!($a,     $c,     $e,         $h; @without_children);
        fed4!($a,         $d, $e,         $h; @without_children);
        fed4!(    $b, $c, $d,             $h; @without_children);
        fed4!(    $b, $c,     $e,         $h; @without_children);
        fed4!(    $b,     $d, $e,         $h; @without_children);
        fed4!(        $c, $d, $e,         $h; @without_children);
        fed4!($a, $b,             $f,     $h; @without_children);
        fed4!($a,     $c,         $f,     $h; @without_children);
        fed4!($a,         $d,     $f,     $h; @without_children);
        fed4!($a,             $e, $f,     $h; @without_children);
        fed4!(    $b, $c,         $f,     $h; @without_children);
        fed4!(    $b,     $d,     $f,     $h; @without_children);
        fed4!(    $b,         $e, $f,     $h; @without_children);
        fed4!(        $c, $d,     $f,     $h; @without_children);
        fed4!(        $c,     $e, $f,     $h; @without_children);
        fed4!(            $d, $e, $f,     $h; @without_children);
        fed4!($a, $b,                 $g, $h; @without_children);
        fed4!($a,     $c,             $g, $h; @without_children);
        fed4!($a,         $d,         $g, $h; @without_children);
        fed4!($a,             $e,     $g, $h; @without_children);
        fed4!($a,                 $f, $g, $h; @without_children);
        fed4!(    $b, $c,             $g, $h; @without_children);
        fed4!(    $b,     $d,         $g, $h; @without_children);
        fed4!(    $b,         $e,     $g, $h; @without_children);
        fed4!(    $b,             $f, $g, $h; @without_children);
        fed4!(        $c, $d,         $g, $h; @without_children);
        fed4!(        $c,     $e,     $g, $h; @without_children);
        fed4!(        $c,         $f, $g, $h; @without_children);
        fed4!(            $d, $e,     $g, $h; @without_children);
        fed4!(            $d,     $f, $g, $h; @without_children);
        fed4!(                $e, $f, $g, $h; @without_children);

        fed3!($a, $b, $c;                     @without_children);
        fed3!($a, $b,     $d;                 @without_children);
        fed3!($a, $b,         $e;             @without_children);
        fed3!($a,     $c, $d;                 @without_children);
        fed3!($a,     $c,     $e;             @without_children);
        fed3!($a,         $d, $e;             @without_children);
        fed3!(    $b, $c, $d;                 @without_children);
        fed3!(    $b, $c,     $e;             @without_children);
        fed3!(    $b,     $d, $e;             @without_children);
        fed3!(        $c, $d, $e;             @without_children);
        fed3!($a, $b,             $f;         @without_children);
        fed3!($a,     $c,         $f;         @without_children);
        fed3!($a,         $d,     $f;         @without_children);
        fed3!($a,             $e, $f;         @without_children);
        fed3!(    $b, $c,         $f;         @without_children);
        fed3!(    $b,     $d,     $f;         @without_children);
        fed3!(    $b,         $e, $f;         @without_children);
        fed3!(        $c, $d,     $f;         @without_children);
        fed3!(        $c,     $e, $f;         @without_children);
        fed3!(            $d, $e, $f;         @without_children);
        fed3!($a, $b,                 $g;     @without_children);
        fed3!($a,     $c,             $g;     @without_children);
        fed3!($a,         $d,         $g;     @without_children);
        fed3!($a,             $e,     $g;     @without_children);
        fed3!($a,                 $f, $g;     @without_children);
        fed3!(    $b, $c,             $g;     @without_children);
        fed3!(    $b,     $d,         $g;     @without_children);
        fed3!(    $b,         $e,     $g;     @without_children);
        fed3!(    $b,             $f, $g;     @without_children);
        fed3!(        $c, $d,         $g;     @without_children);
        fed3!(        $c,     $e,     $g;     @without_children);
        fed3!(        $c,         $f, $g;     @without_children);
        fed3!(            $d, $e,     $g;     @without_children);
        fed3!(            $d,     $f, $g;     @without_children);
        fed3!(                $e, $f, $g;     @without_children);
        fed3!($a, $b,                     $h; @without_children);
        fed3!($a,     $c,                 $h; @without_children);
        fed3!($a,         $d,             $h; @without_children);
        fed3!($a,             $e,         $h; @without_children);
        fed3!($a,                 $f,     $h; @without_children);
        fed3!($a,                     $g, $h; @without_children);
        fed3!(    $b, $c,                 $h; @without_children);
        fed3!(    $b,     $d,             $h; @without_children);
        fed3!(    $b,         $e,         $h; @without_children);
        fed3!(    $b,             $f,     $h; @without_children);
        fed3!(    $b,                 $g, $h; @without_children);
        fed3!(        $c, $d,             $h; @without_children);
        fed3!(        $c,     $e,         $h; @without_children);
        fed3!(        $c,         $f,     $h; @without_children);
        fed3!(        $c,             $g, $h; @without_children);
        fed3!(            $d, $e,         $h; @without_children);
        fed3!(            $d,     $f,     $h; @without_children);
        fed3!(            $d,         $g, $h; @without_children);
        fed3!(                $e, $f,     $h; @without_children);
        fed3!(                $e,     $g, $h; @without_children);
        fed3!(                    $f, $g, $h; @without_children);

        fed2!($a, $b;                         @without_children);
        fed2!($a,     $c;                     @without_children);
        fed2!($a,         $d;                 @without_children);
        fed2!($a,             $e;             @without_children);
        fed2!($a,                 $f;         @without_children);
        fed2!($a,                     $g;     @without_children);
        fed2!($a,                         $h; @without_children);
        fed2!(    $b, $c;                     @without_children);
        fed2!(    $b,     $d;                 @without_children);
        fed2!(    $b,         $e;             @without_children);
        fed2!(    $b,             $f;         @without_children);
        fed2!(    $b,                 $g;     @without_children);
        fed2!(    $b,                     $h; @without_children);
        fed2!(        $c, $d;                 @without_children);
        fed2!(        $c,     $e;             @without_children);
        fed2!(        $c,         $f;         @without_children);
        fed2!(        $c,             $g;     @without_children);
        fed2!(        $c,                 $h; @without_children);
        fed2!(            $d, $e;             @without_children);
        fed2!(            $d,     $f;         @without_children);
        fed2!(            $d,         $g;     @without_children);
        fed2!(            $d,             $h; @without_children);
        fed2!(                $e, $f;         @without_children);
        fed2!(                $e,     $g;     @without_children);
        fed2!(                $e,         $h; @without_children);
        fed2!(                    $f, $g;     @without_children);
        fed2!(                    $f,     $h; @without_children);
        fed2!(                        $g, $h; @without_children);

        fed8!($newtype; $a, $b, $c, $d, $e, $f, $g, $h; @without_children);
    };
}

/// Moar examples that don't actually work
/// ```rust,ignore
///     fn dumb_thing_3(b: Fed2<B1, B7>) {
///         let message = String::from("It's a ") + match b {
///             _: B1 => "B1",
///             _: B2 => "B2",
///         } + "!";
///
///         println!("{}", message);
///     }
/// ```
///
/// ```rust,ignore
///     fn dumb_thing_4(b: Fed2<bool, Option<u8>>) {
///         let debugged = match b {
///             Some(0) |
///             None    |
///             false   => "nothing",
///
///             Some(1) |
///             true    => "one-an-only",
///
///             Some(_) => "many",
///         };
///
///         println!("It's a {}.", debugged);
///     }
/// ```
///
/// ```rust,ignore
///     fn dumb_thing_5(b: Fed2<bool, Option<bool>>) {
///         let result = match b {
///             Some(condition) |
///             condition: bool => condition,
///
///             None => false,
///         };
///
///         println!("It's {:?}.", result);
///     }
/// ```
///
/// ```rust,ignore
///     fn dumb_thing_6(b: Fed2<u8, i8>) {
///         let Ok(magnitude) = b.map(|i: i8| i.abs() as u8).extract::<u8>();
///
///         println!("It's {:?}.", magnitude);
///     }
/// ```
///
/// ```rust,ignore
///     fn dumb_thing_7(b: Fed2<u8, i8>) {
///         let Ok(is_non_negative) = b.map(|_: u8| true)
///             .map(|i: i8| i > 0)
///             .extract::<bool>();
///
///         println!("Claim: It is non-negative... {:?}.", non_negative);
///     }
/// ```
///
/// ```rust,ignore
///     fn dumb_thing_8(b: (bool, u8)) -> Fed2<u8, i16> {
///         match b {
///             (true, i) => ((i as i16) * -1).into(),
///             (false, i) => i.into(),
///         }
///     }
/// ```

#[cfg(test)]
mod test {
    init_fed!();
    use self::fed::*;

    macro_rules! fed_vec {
        ($($element:expr),*,) => { vec![
            $( ($element).into()),*
        ] };
    }

    fed!(
        String,
        Vec<bool>,
        bool,
        i64,
        u8
    );

    fed!(
        (),
        Vec<Vec<()>>,
        char,
        f32,
        f64
    );

    #[test]
    fn test_map_all() {
        let var: Fed1<String> = String::from("abc").into();
        assert_eq!(
            var.map_all(
                |string: String| string.contains('b')
            ),
            true
        );

        let var: Fed3<String, bool, u8> = String::from("abc").into();
        assert_eq!(
            var.map_all(
                |string:  String| string.contains('b'),
                |boolean: bool|   boolean,
                |_:       u8|     false,
            ),
            true
        );

        let var: Fed3<String, bool, u8> = String::from("abc").into();
        let var: Fed2<bool, u8> = var.map_all(
            |string:  String| string.contains('b').into(),
            |boolean: bool  | boolean.into(),
            |num:     u8    | num.into(),
        );
        assert_eq!(var, true.into());
    }

    #[test]
    fn test_as_ref() {
        let val: Fed3<String, bool, u8> = String::from("abc").into();
        assert_eq!(val.as_ref(), (&String::from("abc")).into());
    }

    #[test]
    fn tester() {
        let vec: Vec<Fed3<bool, i64, u8>> = vec![
            false.into(),
            27_i64.into(),
            0_i64.into(),
            b'A'.into(),
        ];

        assert_eq!(vec.iter().filter(Fed::is::<i64>).count(), 2);
        assert_eq!(vec[3].extract::<u8>(), Ok(b'A'));
    }

    #[test]
    fn fed3() {
        let element: Fed3<String, bool, u8> = 8u8.into();
        let element = element.map_same(|element: u8| element + 83);
        assert!(element.is::<u8>());
        assert_eq!(element, (8u8 + 83).into());
        assert!(!element.is::<String>());

        let element: Fed3<String, bool, u8> = String::from("abc").into();
        let element = element.map_same(|element: u8| element + 83);
        assert!(!element.is::<u8>());
        assert_eq!(element, String::from("abc").into());
        assert!(element.is::<String>());

        let element = match element.extract::<String>() {
            ::std::result::Result::Ok(el) => el,
            ::std::result::Result::Err(_) => loop {},
        };

        assert_eq!(element, String::from("abc"));

        let vec: Vec<Fed3<String, bool, u8>> = vec!(8u8.into(), false.into());
        assert_eq!(vec.iter().filter(Fed::is::<u8>).count(), 1);
        assert_eq!(vec.last(), Some(&false.into()));
    }

    #[test]
    fn fed1() {
        let element: Fed1<_> = 3u8.into();
        let element = element.extract::<u8>().ok().unwrap();
        // If Rust fully understood the 'never' type, we might be able to do this instead:
        // let ::std::result::Result::Ok(element) = element.extract::<u8>();
        assert_eq!(element, 3u8);
    }

    #[derive(Copy, Clone, Debug, PartialEq)]
    struct B1;

    #[derive(Copy, Clone, Debug, PartialEq)]
    struct B2;

    #[derive(Copy, Clone, Debug, PartialEq)]
    struct B3;

    #[derive(Copy, Clone, Debug, PartialEq)]
    struct B4;

    fed!(
        B1,
        B2,
        B3,
        B4,
    );

    fn dumb_thing_1(b: Fed2<B1, B3>) -> bool {
        b.is::<B1>()
    }

    #[test]
    fn try_dumb_thing_1() {
        assert!(dumb_thing_1(B1.into()));
        assert!(!dumb_thing_1(B3.into()));
    }

    fn dumb_thing_2<T: Into<Fed2<B1, B3>>>(b: T) -> bool {
        Fed2::<B1, B3>::is::<B1>(&b.into())
    }

    #[test]
    fn try_dumb_thing_2() {
        assert!(dumb_thing_2(B1));
        assert!(!dumb_thing_2(B3));
    }

    fn dumb_thing_3<T: Into<Fed2<B1, B3>>>(b: T) -> Fed2<B1, B3> {
        b.into()
    }

    #[test]
    fn try_dumb_thing_3() {
        let vec: Vec<Fed3<B1, B2, B3>> = vec![
            dumb_thing_3(B3).into(),
            dumb_thing_3(B1).into(),
            dumb_thing_3(B1).into(),
            B3.into(),
            Fed1::<B1>::from(B1).into(),
        ];

        assert_eq!(vec.iter().filter(Fed::is::<B1>).count(), 3);
        assert_eq!(vec.iter().filter(Fed::is::<B2>).count(), 0);
        assert_eq!(vec.iter().filter(Fed::is::<B3>).count(), 2);

        let vec2 = fed_vec![
            dumb_thing_3(B3),
            dumb_thing_3(B1),
            dumb_thing_3(B1),
            B3,
            Fed1::from(B1),
        ];

        assert_eq!(vec, vec2);
    }

    #[test]
    fn test_map_all2() {
        let var: Fed4<B1, B2, B3, B4> = B2.into();

        assert_eq!(var, B2.into());

        let mapped: Fed2<B1, B3> = var.map_all(
            |b: B1| b.into(),
            |_: B2| B3.into(),
            |b: B3| b.into(),
            |_: B4| B3.into(),
        );
        assert_eq!(mapped, B3.into());

        let var2: Fed3<B1, B2, B4> = B2.into();
        assert_eq!(var, var2.into());
    }

    #[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
    struct Nothing;

    type Maybe<T> = Fed2<T, Nothing>;

    fed!(bool, Nothing);
    fed!(String, Nothing);
    fed!(u8, Nothing);

    trait MaybeConstraint<T>: From<T> + From<Nothing> {}
    impl<T> MaybeConstraint<T> for Maybe<T> where Maybe<T>: From<T> + From<Nothing> {}

    impl<T> Default for Maybe<T>
    where Self: MaybeConstraint<T>
    {
        fn default() -> Self {
            Nothing.into()
        }
    }


    impl<T> Maybe<T>
    where Self: MaybeConstraint<T>
    {
        pub fn map<U, F>(self, verb: F) -> Maybe<U>
        where
            Maybe<U>: MaybeConstraint<U>,
            F: FnOnce(T) -> U,
        {
            self.map_all(
                |some: T| verb(some).into(),
                |_|       Nothing.into(),
            )
        }

        pub fn unwrap(self) -> T {
            self.map_all(
                |t| t,
                |_: Nothing| panic!("called `Maybe<_>::unwrap()` on a `Nothing` value"),
            )
        }
    }

    impl<T> Into<Option<T>> for Maybe<T>
    where Self: MaybeConstraint<T>
    {
        fn into(self) -> Option<T> {
            self.map_all(Some, |_| None)
        }
    }

    #[test]
    fn fake_option() {
        let options: Vec<Maybe<_>> = fed_vec![
            Nothing,
            true,
            false,
        ];

        let options2: Vec<_> = options.iter().cloned()
            .map(|option| {
                option.map(|boolean| boolean.to_string())
            }).collect();

        let options3: Vec<Maybe<_>> = fed_vec![
            Nothing,
            String::from("true"),
            String::from("false"),
        ];

        assert_eq!(options2, options3);

        let maybe_num: Maybe<_> = 33.into();
        assert_eq!(maybe_num, 33.into());
        assert_eq!(maybe_num.unwrap(), 33);
    }

    pub struct Value(pub ValueT);
    type ValueT = Fed3<i32, String, Array>;
    type Array = Vec<Value>;
    fed!(
        i32,
        String,
        Array,
    );

    use ::std::ops::Deref;
    impl Deref for Value {
        type Target = ValueT;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl From<ValueT> for Value {
        fn from(value: ValueT) -> Self {
            Value(value)
        }
    }

    #[test]
    fn recursive() {
        let value: Value = ValueT::from(123).into();

        assert!((*value).is::<i32>());
    }
}
