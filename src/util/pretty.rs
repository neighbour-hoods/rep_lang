use pretty::RcDoc;

#[macro_export]
macro_rules! sp {
    () => {
        RcDoc::text(" ")
    };
}

pub fn parens<T>(doc: RcDoc<T>) -> RcDoc<T> {
    RcDoc::text("(").append(doc).append(RcDoc::text(")"))
}

pub fn to_pretty(doc: RcDoc<()>, width: usize) -> String {
    let mut w = Vec::new();
    doc.render(width, &mut w).unwrap();
    String::from_utf8(w).unwrap()
}
