#[macro_use]
extern crate fed as fed_;

#[macro_use]
extern crate serde_derive;
extern crate bincode;

use bincode::{serialize, deserialize, Infinite};

init_fed!(@deriving:[Serialize, Deserialize]);
use fed::*;

fed!(
    usize,
    bool,
    Option<String>,
    char,
);

fn main() {
    let a: Fed4<_,_,_,_> = 42.into();

    let encoded = serialize(&a, Infinite).unwrap();

    let decoded = deserialize(&encoded[..]).unwrap();

    assert_eq!(a, decoded);
}
