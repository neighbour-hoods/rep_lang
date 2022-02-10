use std::{collections::hash_map::DefaultHasher, hash::Hash, hash::Hasher};

// taken from https://doc.rust-lang.org/std/hash/index.html
pub fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

#[cfg(feature = "hdk")]
#[macro_export]
macro_rules! error {
    ($($arg:tt)*) => ({{
        panic!("check Holochain error output for complete error. {:?}", hdk::prelude::error!($($arg)*))
    }});
}

#[cfg(not(feature = "hdk"))]
#[macro_export]
macro_rules! error {
    ($($arg:tt)*) => {
        panic!($($arg)*)
    };
}
