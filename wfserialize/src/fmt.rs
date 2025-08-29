use alloc::vec::Vec;
use core::alloc::Allocator;
use core::fmt::Write;

use arrayvec::ArrayString;

pub struct Formatter<A: Allocator> {
    // TODO(yan): String<A>, once it exists.
    data: Vec<u8, A>,
    indent: u32,
}

impl<A: Allocator> Formatter<A> {
    pub fn new_in(allocator: A) -> Self {
        Self {
            data: Vec::new_in(allocator),
            indent: 0,
        }
    }

    pub fn into_data(self) -> Vec<u8, A> {
        self.data
    }

    pub fn null(&mut self) {
        self.data.extend("null".as_bytes());
    }

    // Not generic, because there's no useful automatic Into<bool> impl.
    pub fn boolean(&mut self, value: bool) {
        if value {
            self.data.extend("true".as_bytes());
        } else {
            self.data.extend("false".as_bytes());
        }
    }

    pub fn float<F: Into<f64>>(&mut self, value: F) {
        let value = value.into();

        let mut buf: ArrayString<256> = ArrayString::new();
        write!(buf, "{value}").unwrap();
        self.data.extend(buf.as_bytes());
    }

    pub fn int<I: Into<i64>>(&mut self, value: I) {
        let value = value.into();

        let mut buf: ArrayString<256> = ArrayString::new();
        write!(buf, "{value}").unwrap();
        self.data.extend(buf.as_bytes());
    }

    pub fn uint<U: Into<u64>>(&mut self, value: U) {
        let value = value.into();

        let mut buf: ArrayString<256> = ArrayString::new();
        write!(buf, "{value}").unwrap();
        self.data.extend(buf.as_bytes());
    }

    // Not generic, because there's no useful automatic Into<&str> impl.
    pub fn string(&mut self, value: &str) {
        self.data.push(b'"');
        self.data.extend(value.as_bytes());
        self.data.push(b'"');
    }

    pub fn unit_struct(&mut self, ident: &str) {
        assert!(check_ident(ident));
        self.data.extend(ident.as_bytes())
    }

    pub fn array<C>(&mut self, multiline: bool, c: C)
    where
        C: Fn(&mut ArrayLikeFormatter<'_, A>),
    {
        self.array_like(multiline, c, None, b'[', b']');
    }

    pub fn tuple<C>(&mut self, multiline: bool, c: C)
    where
        C: Fn(&mut ArrayLikeFormatter<'_, A>),
    {
        self.array_like(multiline, c, None, b'(', b')');
    }

    pub fn tuple_struct<C>(&mut self, ident: &str, multiline: bool, c: C)
    where
        C: Fn(&mut ArrayLikeFormatter<'_, A>),
    {
        self.array_like(multiline, c, Some(ident), b'(', b')');
    }

    pub fn dictionary<C>(&mut self, multiline: bool, c: C)
    where
        C: Fn(&mut DictionaryLikeFormatter<'_, A>),
    {
        self.dictionary_like(multiline, c, None, None);
    }

    pub fn dictionary_struct<C>(&mut self, ident: &str, multiline: bool, c: C)
    where
        C: Fn(&mut DictionaryLikeFormatter<'_, A>),
    {
        self.dictionary_like(multiline, c, Some(ident), None);
    }

    pub fn dictionary_struct_with_version<C>(&mut self, ident: &str, version: u64, multiline: bool, c: C)
    where
        C: Fn(&mut DictionaryLikeFormatter<'_, A>),
    {
        self.dictionary_like(multiline, c, Some(ident), Some(version));
    }

    fn array_like<C>(&mut self, multiline: bool, c: C, ident: Option<&str>, open_char: u8, close_char: u8)
    where
        C: Fn(&mut ArrayLikeFormatter<'_, A>),
    {
        if let Some(ident) = ident {
            assert!(check_ident(ident));
            self.data.extend(ident.as_bytes());
        }

        self.data.push(open_char);
        if multiline {
            self.indent += 2;
        }

        let mut af = ArrayLikeFormatter {
            f: self,
            multiline,
            first: true,
        };
        c(&mut af);
        let first = af.first;

        if multiline {
            self.indent -= 2;
            if !first {
                self.newline();
            }
        }
        self.data.push(close_char);
    }

    fn dictionary_like<C>(&mut self, multiline: bool, c: C, ident: Option<&str>, version: Option<u64>)
    where
        C: Fn(&mut DictionaryLikeFormatter<'_, A>),
    {
        if let Some(ident) = ident {
            assert!(check_ident(ident));
            self.data.extend(ident.as_bytes());
            self.data.push(b' ');
        }

        if let Some(version) = version {
            self.data.push(b'#');
            self.uint(version);
            self.data.push(b' ');
        }

        self.data.push(b'{');
        if multiline {
            self.indent += 2;
        }

        let mut of = DictionaryLikeFormatter {
            f: self,
            multiline,
            fields_are_idents: ident.is_some(),
            first: true,
        };
        c(&mut of);
        let first = of.first;

        if multiline {
            self.indent -= 2;
            if !first {
                self.newline();
            }
        } else if !first {
            self.data.push(b' ');
        }
        self.data.push(b'}');
    }

    fn newline(&mut self) {
        self.data.push(b'\n');
        for _ in 0..self.indent {
            self.data.push(b' ');
        }
    }
}

pub struct DictionaryLikeFormatter<'a, A: Allocator> {
    f: &'a mut Formatter<A>,
    multiline: bool,
    fields_are_idents: bool,
    first: bool,
}

impl<A: Allocator> DictionaryLikeFormatter<'_, A> {
    pub fn field<C>(&mut self, key: &str, c: C)
    where
        C: Fn(&mut Formatter<A>),
    {
        self.pre_field(key);

        c(self.f);

        self.post_field();
        self.first = false;
    }

    pub fn null_field(&mut self, key: &str) {
        self.pre_field(key);
        self.f.null();
        self.post_field();
        self.first = false;
    }

    pub fn boolean_field(&mut self, key: &str, value: bool) {
        self.pre_field(key);
        self.f.boolean(value);
        self.post_field();
        self.first = false;
    }

    pub fn float_field<F: Into<f64>>(&mut self, key: &str, value: F) {
        self.pre_field(key);
        self.f.float(value);
        self.post_field();
        self.first = false;
    }

    pub fn int_field<I: Into<i64>>(&mut self, key: &str, value: I) {
        self.pre_field(key);
        self.f.int(value);
        self.post_field();
        self.first = false;
    }

    pub fn uint_field<U: Into<u64>>(&mut self, key: &str, value: U) {
        self.pre_field(key);
        self.f.uint(value);
        self.post_field();
        self.first = false;
    }

    pub fn string_field(&mut self, key: &str, value: &str) {
        self.pre_field(key);
        self.f.string(value);
        self.post_field();
        self.first = false;
    }

    pub fn unit_struct_field(&mut self, key: &str, value_ident: &str) {
        self.pre_field(key);
        self.f.unit_struct(value_ident);
        self.post_field();
        self.first = false;
    }

    pub fn array_field<C>(&mut self, key: &str, multiline: bool, c: C)
    where
        C: Fn(&mut ArrayLikeFormatter<'_, A>),
    {
        self.pre_field(key);
        self.f.array(multiline, c);
        self.post_field();
        self.first = false;
    }

    pub fn tuple_field<C>(&mut self, key: &str, multiline: bool, c: C)
    where
        C: Fn(&mut ArrayLikeFormatter<'_, A>),
    {
        self.pre_field(key);
        self.f.tuple(multiline, c);
        self.post_field();
        self.first = false;
    }

    pub fn tuple_struct_field<C>(&mut self, key: &str, ident: &str, multiline: bool, c: C)
    where
        C: Fn(&mut ArrayLikeFormatter<'_, A>),
    {
        self.pre_field(key);
        self.f.tuple_struct(ident, multiline, c);
        self.post_field();
        self.first = false;
    }

    pub fn dictionary_field<C>(&mut self, key: &str, multiline: bool, c: C)
    where
        C: Fn(&mut DictionaryLikeFormatter<'_, A>),
    {
        self.pre_field(key);
        self.f.dictionary(multiline, c);
        self.post_field();
        self.first = false;
    }

    pub fn dictionary_struct_field<C>(&mut self, key: &str, ident: &str, multiline: bool, c: C)
    where
        C: Fn(&mut DictionaryLikeFormatter<'_, A>),
    {
        self.pre_field(key);
        self.f.dictionary_struct(ident, multiline, c);
        self.post_field();
        self.first = false;
    }

    fn pre_field(&mut self, key: &str) {
        if self.multiline {
            self.f.newline();
        } else {
            if self.first {
                self.f.data.push(b' ');
            } else {
                self.f.data.push(b',');
                self.f.data.push(b' ');
            }
        }

        if self.fields_are_idents {
            assert!(check_ident(key));
            self.f.data.extend(key.as_bytes());
        } else {
            self.f.string(key);
        }

        self.f.data.push(b':');
        self.f.data.push(b' ');
    }

    fn post_field(&mut self) {
        if self.multiline {
            self.f.data.push(b',');
        }
    }
}

pub struct ArrayLikeFormatter<'a, A: Allocator> {
    f: &'a mut Formatter<A>,
    multiline: bool,
    first: bool,
}

impl<A: Allocator> ArrayLikeFormatter<'_, A> {
    pub fn item<C>(&mut self, c: C)
    where
        C: Fn(&mut Formatter<A>),
    {
        self.pre_item();

        c(self.f);

        self.post_item();
        self.first = false;
    }

    pub fn null_item(&mut self) {
        self.pre_item();
        self.f.null();
        self.post_item();
        self.first = false;
    }

    pub fn boolean_item(&mut self, value: bool) {
        self.pre_item();
        self.f.boolean(value);
        self.post_item();
        self.first = false;
    }

    pub fn float_item<F: Into<f64>>(&mut self, value: F) {
        self.pre_item();
        self.f.float(value);
        self.post_item();
        self.first = false;
    }

    pub fn int_item<I: Into<i64>>(&mut self, value: I) {
        self.pre_item();
        self.f.int(value);
        self.post_item();
        self.first = false;
    }

    pub fn uint_item<U: Into<u64>>(&mut self, value: U) {
        self.pre_item();
        self.f.uint(value);
        self.post_item();
        self.first = false;
    }

    pub fn string_item(&mut self, value: &str) {
        self.pre_item();
        self.f.string(value);
        self.post_item();
        self.first = false;
    }

    pub fn unit_struct_item(&mut self, ident: &str) {
        self.pre_item();
        self.f.unit_struct(ident);
        self.post_item();
        self.first = false;
    }

    pub fn array_item<C>(&mut self, multiline: bool, c: C)
    where
        C: Fn(&mut ArrayLikeFormatter<'_, A>),
    {
        self.pre_item();
        self.f.array(multiline, c);
        self.post_item();
        self.first = false;
    }

    pub fn tuple_item<C>(&mut self, multiline: bool, c: C)
    where
        C: Fn(&mut ArrayLikeFormatter<'_, A>),
    {
        self.pre_item();
        self.f.tuple(multiline, c);
        self.post_item();
        self.first = false;
    }

    pub fn tuple_struct_item<C>(&mut self, ident: &str, multiline: bool, c: C)
    where
        C: Fn(&mut ArrayLikeFormatter<'_, A>),
    {
        self.pre_item();
        self.f.tuple_struct(ident, multiline, c);
        self.post_item();
        self.first = false;
    }

    pub fn dictionary_item<C>(&mut self, multiline: bool, c: C)
    where
        C: Fn(&mut DictionaryLikeFormatter<'_, A>),
    {
        self.pre_item();
        self.f.dictionary(multiline, c);
        self.post_item();
        self.first = false;
    }

    pub fn dictionary_struct_item<C>(&mut self, ident: &str, multiline: bool, c: C)
    where
        C: Fn(&mut DictionaryLikeFormatter<'_, A>),
    {
        self.pre_item();
        self.f.dictionary_struct(ident, multiline, c);
        self.post_item();
        self.first = false;
    }

    fn pre_item(&mut self) {
        if self.multiline {
            self.f.newline();
        }

        if !self.multiline && !self.first {
            self.f.data.push(b',');
            self.f.data.push(b' ');
        }
    }

    fn post_item(&mut self) {
        if self.multiline {
            self.f.data.push(b',');
        }
    }
}

fn check_ident(ident: &str) -> bool {
    if ident.is_empty() {
        return false;
    }

    let mut chars = ident.chars();
    let c = chars.next().unwrap();
    if !is_ident_start_char(c) {
        return false;
    }

    for c in chars {
        if !is_ident_char(c) {
            return false;
        }
    }

    true
}

fn is_ident_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

fn is_ident_start_char(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

#[cfg(test)]
mod tests {
    use alloc::alloc::Global;
    use core::str;

    use super::*;

    #[test]
    fn test_array_empty() {
        let mut f = Formatter::new_in(Global);
        f.array(false, |_| ());

        let output = str::from_utf8(&f.data).unwrap();
        assert!(output == "[]");
    }

    #[test]
    fn test_array_empty_multiline() {
        let mut f = Formatter::new_in(Global);
        f.array(true, |_| ());

        let output = str::from_utf8(&f.data).unwrap();
        assert!(output == "[]");
    }

    #[test]
    fn test_array() {
        let mut f = Formatter::new_in(Global);
        f.array(true, |af| {
            af.boolean_item(true);
            af.float_item(6.96);
            af.string_item("Sailor");
            af.array_item(false, |aaf| {
                aaf.null_item();
            });
            af.dictionary_item(false, |of| {
                of.boolean_field("nested", true);
            });
        });

        let str = r#"[
  true,
  6.96,
  "Sailor",
  [null],
  { "nested": true },
]"#;

        let output = str::from_utf8(&f.data).unwrap();
        assert!(output == str);
    }

    // TODO(yan): Test tuples.

    #[test]
    fn test_tuple_struct() {
        let mut f = Formatter::new_in(Global);
        f.tuple_struct("Chilli", true, |af| {
            af.boolean_item(true);
            af.float_item(6.96);
            af.unit_struct_item("Sailor");
            af.array_item(false, |aaf| {
                aaf.null_item();
            });
            af.dictionary_item(false, |of| {
                of.boolean_field("nested", true);
            });
        });

        let str = r#"Chilli(
  true,
  6.96,
  Sailor,
  [null],
  { "nested": true },
)"#;

        let output = str::from_utf8(&f.data).unwrap();
        assert!(output == str);
    }

    #[test]
    fn test_dictionary_empty() {
        let mut f = Formatter::new_in(Global);
        f.dictionary(false, |_| ());

        let output = str::from_utf8(&f.data).unwrap();
        assert!(output == "{}");
    }

    #[test]
    fn test_dictionary_empty_multiline() {
        let mut f = Formatter::new_in(Global);
        f.dictionary(true, |_| ());

        let output = str::from_utf8(&f.data).unwrap();
        assert!(output == "{}");
    }

    #[test]
    fn test_dictionary() {
        let mut f = Formatter::new_in(Global);
        f.dictionary(true, |of| {
            of.boolean_field("yes", true);
            of.float_field("Favorite Number", 6.96);
            of.string_field("Hello", "Sailor");
            of.dictionary_field("innerdata", false, |oof| {
                oof.int_field("employee age", 9000);
            });
            of.array_field("arraydata", false, |af| {
                af.boolean_item(false);
                af.boolean_item(true);
                af.int_item(42);
            });
            of.array_field("morearraydata", true, |af| {
                af.int_item(0);
                af.array_item(false, |aaf| {
                    aaf.null_item();
                });
                af.dictionary_item(false, |oof| {
                    oof.string_field("I am", "therefore I");
                });
                af.int_item(1);
            });
        });

        let str = r#"{
  "yes": true,
  "Favorite Number": 6.96,
  "Hello": "Sailor",
  "innerdata": { "employee age": 9000 },
  "arraydata": [false, true, 42],
  "morearraydata": [
    0,
    [null],
    { "I am": "therefore I" },
    1,
  ],
}"#;

        let output = str::from_utf8(&f.data).unwrap();
        assert!(output == str);
    }

    #[test]
    fn test_dictionary_struct() {
        let mut f = Formatter::new_in(Global);
        f.dictionary_struct("Vanilla", true, |of| {
            of.boolean_field("yes", true);
            of.float_field("Favorite_Number", 6.96);
            of.string_field("Hello", "Sailor");
            of.dictionary_field("innerdata", false, |oof| {
                oof.int_field("employee age", 9000);
            });
            of.array_field("arraydata", false, |af| {
                af.boolean_item(false);
                af.boolean_item(true);
                af.int_item(42);
            });
            of.array_field("morearraydata", true, |af| {
                af.int_item(0);
                af.array_item(false, |aaf| {
                    aaf.null_item();
                });
                af.dictionary_item(false, |oof| {
                    oof.string_field("I am", "therefore I");
                });
                af.unit_struct_item("Am");
            });
        });

        let str = r#"Vanilla {
  yes: true,
  Favorite_Number: 6.96,
  Hello: "Sailor",
  innerdata: { "employee age": 9000 },
  arraydata: [false, true, 42],
  morearraydata: [
    0,
    [null],
    { "I am": "therefore I" },
    Am,
  ],
}"#;

        let output = str::from_utf8(&f.data).unwrap();
        assert!(output == str);
    }

    #[test]
    fn test_dictionary_struct_with_version() {
        let mut f = Formatter::new_in(Global);
        f.dictionary_struct_with_version("Vanilla", 420, true, |of| {
            of.boolean_field("yes", true);
            of.float_field("Favorite_Number", 6.96);
            of.string_field("Hello", "Sailor");
            of.dictionary_field("innerdata", false, |oof| {
                oof.int_field("employee age", 9000);
            });
            of.array_field("arraydata", false, |af| {
                af.boolean_item(false);
                af.boolean_item(true);
                af.int_item(42);
            });
            of.array_field("morearraydata", true, |af| {
                af.int_item(0);
                af.array_item(false, |aaf| {
                    aaf.null_item();
                });
                af.dictionary_item(false, |oof| {
                    oof.string_field("I am", "therefore I");
                });
                af.unit_struct_item("Am");
            });
        });

        let str = r#"Vanilla #420 {
  yes: true,
  Favorite_Number: 6.96,
  Hello: "Sailor",
  innerdata: { "employee age": 9000 },
  arraydata: [false, true, 42],
  morearraydata: [
    0,
    [null],
    { "I am": "therefore I" },
    Am,
  ],
}"#;

        let output = str::from_utf8(&f.data).unwrap();
        assert!(output == str);
    }

    #[test]
    #[should_panic]
    fn test_invalid_ident_unit_struct() {
        let mut f = Formatter::new_in(Global);
        f.unit_struct("I am become panic");
    }

    #[test]
    #[should_panic]
    fn test_invalid_ident_tuple_struct() {
        let mut f = Formatter::new_in(Global);
        f.tuple_struct("I am become panic", false, |_| {});
    }

    #[test]
    #[should_panic]
    fn test_invalid_ident_dictionary_struct() {
        let mut f = Formatter::new_in(Global);
        f.dictionary_struct("I am become panic", false, |_| {});
    }

    #[test]
    #[should_panic]
    fn test_invalid_ident_dictionary_struct_field() {
        let mut f = Formatter::new_in(Global);
        f.dictionary_struct("Fuckaroo", false, |sf| {
            sf.null_field("#mate");
        });
    }
}
