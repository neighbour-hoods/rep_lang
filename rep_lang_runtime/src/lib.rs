//! `rep_lang` interpreter functionality.

// TODO should this go here?
#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

pub mod env;
pub mod eval;
pub mod infer;
pub mod types;

pub mod test;
