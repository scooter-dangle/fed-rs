fed
===

Usage
-----
In your Cargo.toml file:

```toml
[dependencies]
fed = "0.2"
```

In order to be able to declare type federations, the various `Fed*` types need
to be owned by your crate.  To enable this, the code declaring the generic
`FedX<*_>` is contained in a macro that the user needs to call in their crate
root:

```rust
#[macro_use]
extern crate fed as fed_;
init_fed!();
```

This will create a `fed` module in your crate root.

Since much of the functionality (especially the `.into()` trick) can currently
only be defined on concrete types, you'll need to explicitly declare a type
federation with the `fed!` macro:

```rust
fed!(
    usize,
    bool,
    Option<String>,
    char,
);
```

This will implement the traits in `::fed::*` for the concrete type
`Fed4<usize, bool, Option<String>, char>` as well as the concrete `Fed2`
and `Fed3` types for all of the 2- and 3-type subsets of `usize`, `bool`,
`Option<String>`, and `char` (e.g., `Fed2<usize, bool>`, `Fed2<usize, char>`,
`Fed3<usize, bool, Option<String>>`).

`Fed1`, since it's a federation of exactly one type, is able to have totally
generic implementations of the `::fed::*` traits.

Three immediately relevant limitations:

1. When using any of the generated concrete type federations and specifying the
types in the federation, the types must be listed in the same order in which
they were declared in the `fed!` call. In the above example, `Fed2<usize, bool>`
is now usable, but `Fed2<bool, usize>` is not.

2. Because Rust forbids duplicate, identical trait implementations, the types
listed in a second `fed!` declaration cannot have a common subset of more than
one type with the set of types in the first type federation (unless you order
the second type federation in such a way that no ordered subset is identical
to an ordered subset of the first type federation (as mentioned in the first
limitation)).

3. Type federations of more than 8 types are not supported.

Now, you can create collections of any subset of the types in the federation you
declared without having to come up with names for a dedicated enum:

```rust
use ::fed::*;

let vec: Vec<Fed3<usize, bool, char>> = vec![
    false.into(),
    27usize.into(),
    0usize.into(),
    'A'.into(),
];

assert_eq!(vec.iter().filter(Fed::is::<usize>).count(), 2);
assert_eq!(vec[3].extract::<char>(), Ok('A'));
```
