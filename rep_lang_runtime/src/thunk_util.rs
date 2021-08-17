use super::eval::{FlatThunk, Thunk, Thunk::*, VRef, Value::*};

// example of a `rep_lang` list, fed by an iterator
pub fn i64_iterator_to_flat_thunk_list<T: 'static>(itr: T) -> Thunk<VRef>
where
    T: Iterator<Item = i64> + Clone,
{
    UnevRust(Box::new(rec_list(itr)))
}

pub fn rec_list<T: 'static>(mut it: T) -> Box<dyn FnMut() -> FlatThunk>
where
    T: Iterator<Item = i64> + Clone,
{
    Box::new(move || {
        match it.next() {
            None => FlatThunk(Ev(VNil)),
            Some(x) => {
                let hd = Box::new(FlatThunk(Ev(VInt(x))));
                // this seems suboptimal, due to excessive cloning, but I'm
                // not sure how to do better --------------------\/
                let tl = Box::new(FlatThunk(UnevRust(rec_list(it.clone()))));
                FlatThunk(Ev(VCons(hd, tl)))
            }
        }
    })
}
