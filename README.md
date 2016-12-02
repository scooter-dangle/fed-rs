fed
===

Usage
-----
In order to be able to declare type federations, the various `Fed*` types need to be owned by your crate.
To enable this, the code declaring the generic `FedX<*_>` is contained in a macro that the user needs
to call in their crate root:

```rust
#[macro_use]
extern crate fed as fed_;
init_fed!();
```
