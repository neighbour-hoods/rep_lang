use byteorder::{BigEndian, ByteOrder};
use std::{
    convert::TryInto,
    fs::File,
    io::{Read, Seek, SeekFrom},
    path::Path,
};

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

pub fn i64_vec_to_flat_thunk_list(vec: Vec<i64>) -> Thunk<VRef> {
    UnevRust(Box::new(rec_vec(vec, 0)))
}

fn rec_vec(vec: Vec<i64>, idx: usize) -> Box<dyn FnMut() -> FlatThunk> {
    Box::new(move || {
        if idx >= vec.len() {
            FlatThunk(Ev(VNil))
        } else {
            let hd = Box::new(FlatThunk(Ev(VInt(vec[idx]))));
            // this seems suboptimal, due to excessive cloning, but I'm
            // not sure how to do better --------------------\/
            let tl = Box::new(FlatThunk(UnevRust(rec_vec(vec.clone(), idx + 1))));
            FlatThunk(Ev(VCons(hd, tl)))
        }
    })
}

pub fn file_byte_to_flat_thunk_list(fp: &Path) -> Thunk<VRef> {
    let mut file = File::open(fp).unwrap();
    UnevRust(Box::new(rec_bytes(&mut file)))
}

pub fn rec_bytes(file: &mut File) -> Box<dyn FnMut() -> FlatThunk + '_> {
    Box::new(move || {
        let mut buf = [0; 8];

        let n = file.read(&mut buf[..]).unwrap();

        if n == 0 {
            FlatThunk(Ev(VNil))
        } else {
            let i = BigEndian::read_i64(&buf);
            let hd = Box::new(FlatThunk(Ev(VInt(i))));
            file.seek(SeekFrom::Current(n.try_into().unwrap()));
            let tl = Box::new(FlatThunk(UnevRust(rec_bytes(file))));
            FlatThunk(Ev(VCons(hd, tl)))
        }
    })
}
