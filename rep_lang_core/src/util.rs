use std::{collections::hash_map::DefaultHasher, hash::Hash, hash::Hasher};

// taken from https://doc.rust-lang.org/std/hash/index.html
pub fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}
