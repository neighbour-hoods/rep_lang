// TODO should this go here?
#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

pub mod env;
pub mod infer;
pub mod parse;
pub mod pretty;
pub mod syntax;
pub mod types;
pub mod util;

pub mod test;
