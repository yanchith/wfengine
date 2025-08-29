use alloc::vec::Vec;
use core::alloc::Allocator;
use core::fmt;
use core::str;
use core::str::FromStr;

use hashbrown::DefaultHashBuilder;
use hashbrown::HashMap;

const ARRAY_LIKE_CAP: usize = 8;
const DICTIONARY_LIKE_CAP: usize = 8;

// TODO(jt): Change Vec<u8, A> to String<A> once it exists.

type ArrayValues<A> = Vec<Node<A>, A>;
type TupleValues<A> = Vec<Node<A>, A>;
type TupleStructValues<A> = Vec<Node<A>, A>;
type DictionaryValues<A> = HashMap<Vec<u8, A>, Node<A>, DefaultHashBuilder, A>;
type DictionaryStructValues<A> = HashMap<Vec<u8, A>, Node<A>, DefaultHashBuilder, A>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Kind {
    Null,
    Boolean,
    Float,
    Int,
    Uint,
    String,
    UnitStruct,
    // TODO(jt): @Speed @Memory Add typed arrays, where the entire array is stored in a flat
    // manner. Let's do this at least for numeric/primitive types, but maybe we can do this for all
    // types?
    Array,
    Tuple,
    TupleStruct,
    Dictionary,
    DictionaryStruct,
}

#[derive(Clone)]
pub enum Node<A: Allocator + Clone> {
    Null,
    Boolean(bool),
    Float(f64),
    Int(i64),
    // The parser only ever outputs uint nodes, if the integer fails to parse the value as signed.
    Uint(u64),
    String(Vec<u8, A>),
    UnitStruct(Vec<u8, A>),
    Array(ArrayValues<A>),
    Tuple(TupleValues<A>),
    TupleStruct(Vec<u8, A>, TupleStructValues<A>),
    Dictionary(DictionaryValues<A>),
    DictionaryStruct(Vec<u8, A>, Option<u64>, DictionaryStructValues<A>),
}

// Manual impl to get impls for allocators that are not debug.
impl<A: Allocator + Clone> fmt::Debug for Node<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Null => write!(f, "Null"),
            Self::Boolean(boolean) => write!(f, "Boolean({boolean})"),
            Self::Float(float) => write!(f, "Float({float})"),
            Self::Int(int) => write!(f, "Int({int})"),
            Self::Uint(uint) => write!(f, "Uint({uint})"),
            Self::String(string) => {
                write!(f, r#""{}""#, unsafe { str::from_utf8_unchecked(string) })
            }
            Self::UnitStruct(ident) => {
                write!(f, "{}", unsafe { str::from_utf8_unchecked(ident) })
            }
            Self::Array(values) => {
                write!(f, "[{values:?}]")
            }
            Self::Tuple(values) => {
                write!(f, "[{values:?}]")
            }
            Self::TupleStruct(ident, values) => {
                write!(f, "{} {:?}", unsafe { str::from_utf8_unchecked(ident) }, values,)
            }
            Self::Dictionary(values) => {
                write!(f, "[{values:?}]")
            }
            Self::DictionaryStruct(ident, version, values) => {
                if let Some(version) = version {
                    write!(f, "{} #{version} {values:?}", unsafe {
                        str::from_utf8_unchecked(ident)
                    })
                } else {
                    write!(f, "{} {values:?}", unsafe { str::from_utf8_unchecked(ident) })
                }
            }
        }
    }
}

impl<A: Allocator + Clone> Node<A> {
    pub fn kind(&self) -> Kind {
        match self {
            Self::Null => Kind::Null,
            Self::Boolean(_) => Kind::Boolean,
            Self::Float(_) => Kind::Float,
            Self::Int(_) => Kind::Int,
            Self::Uint(_) => Kind::Uint,
            Self::String(_) => Kind::String,
            Self::UnitStruct(_) => Kind::UnitStruct,
            Self::Array(_) => Kind::Array,
            Self::Tuple(_) => Kind::Tuple,
            Self::TupleStruct(_, _) => Kind::TupleStruct,
            Self::Dictionary(_) => Kind::Dictionary,
            Self::DictionaryStruct(_, _, _) => Kind::DictionaryStruct,
        }
    }

    pub fn is_null(&self) -> bool {
        match self {
            Self::Null => true,
            _ => false,
        }
    }

    pub fn boolean(&self) -> Option<bool> {
        match self {
            Self::Boolean(b) => Some(*b),
            _ => None,
        }
    }

    pub fn float(&self) -> Option<f64> {
        match self {
            Self::Float(f) => Some(*f),
            _ => None,
        }
    }

    pub fn int(&self) -> Option<i64> {
        match self {
            Self::Int(i) => Some(*i),
            Self::Uint(u) => i64::try_from(*u).ok(),
            _ => None,
        }
    }

    pub fn uint(&self) -> Option<u64> {
        match self {
            Self::Int(i) => u64::try_from(*i).ok(),
            Self::Uint(u) => Some(*u),
            _ => None,
        }
    }

    pub fn string(&self) -> Option<&str> {
        match self {
            Self::String(s) => Some(unsafe { str::from_utf8_unchecked(s) }),
            _ => None,
        }
    }

    pub fn array(&self) -> Option<&[Node<A>]> {
        match self {
            Self::Array(a) => Some(a),
            _ => None,
        }
    }

    pub fn dictionary(&self) -> Option<&DictionaryValues<A>> {
        match self {
            Self::Dictionary(o) => Some(o),
            _ => None,
        }
    }
}

// Custom PartialEq, because derive would require the A: PartialEq, which we don't want to force.
impl<A: Allocator + Clone> PartialEq for Node<A> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Null, Self::Null) => true,
            (Self::Boolean(a), Self::Boolean(b)) => a == b,
            (Self::Float(a), Self::Float(b)) => a == b,
            (Self::Int(a), Self::Int(b)) => a == b,
            (Self::Uint(a), Self::Uint(b)) => a == b,
            (Self::String(a), Self::String(b)) => a == b,
            (Self::UnitStruct(aty), Self::UnitStruct(bty)) => aty == bty,
            (Self::Array(a), Self::Array(b)) => a == b,
            (Self::Tuple(a), Self::Tuple(b)) => a == b,
            (Self::TupleStruct(aty, a), Self::TupleStruct(bty, b)) => aty == bty && a == b,
            (Self::Dictionary(a), Self::Dictionary(b)) => a == b,
            (Self::DictionaryStruct(aty, aver, a), Self::DictionaryStruct(bty, bver, b)) => {
                aty == bty && aver == bver && a == b
            }
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedEof { line: u32 },
    UnexpectedToken { line: u32, token: Token<'static> },
    UnexpectedChar { line: u32, c: char },
}

pub fn parse_objects_in<A>(s: &str, allocator: A) -> Result<Node<A>, ParseError>
where
    A: Allocator + Clone,
{
    let mut lexer = Lexer { s, line: 1 };
    parse_node(&mut lexer, allocator)
}

fn parse_node<A>(lexer: &mut Lexer<'_>, allocator: A) -> Result<Node<A>, ParseError>
where
    A: Allocator + Clone,
{
    match lexer.eat_token()? {
        Token::LBrace => {
            let mut v = DictionaryValues::with_capacity_in(DICTIONARY_LIKE_CAP, allocator.clone());

            lexer.maybe_eat_whitespace_and_comments()?;

            while !lexer.peek_rbrace() {
                let key_str = lexer.eat_string()?;
                let key = string_in(key_str, allocator.clone());

                lexer.maybe_eat_whitespace_and_comments()?;
                lexer.eat_colon()?;
                lexer.maybe_eat_whitespace_and_comments()?;

                let node = parse_node(lexer, allocator.clone())?;
                v.insert(key, node);

                lexer.maybe_eat_whitespace_and_comments()?;

                if !lexer.peek_comma() {
                    break;
                }

                lexer.eat_comma().unwrap();
                lexer.maybe_eat_whitespace_and_comments()?;
            }

            lexer.eat_rbrace()?;

            Ok(Node::Dictionary(v))
        }
        Token::LParen => {
            let mut v = Vec::with_capacity_in(ARRAY_LIKE_CAP, allocator.clone());

            lexer.maybe_eat_whitespace_and_comments()?;

            while !lexer.peek_rparen() {
                let node = parse_node(lexer, allocator.clone())?;
                v.push(node);

                lexer.maybe_eat_whitespace_and_comments()?;

                if !lexer.peek_comma() {
                    break;
                }

                lexer.eat_comma().unwrap();
                lexer.maybe_eat_whitespace_and_comments()?;
            }

            lexer.eat_rparen()?;

            Ok(Node::Tuple(v))
        }
        Token::LBracket => {
            let mut v = Vec::with_capacity_in(ARRAY_LIKE_CAP, allocator.clone());

            lexer.maybe_eat_whitespace_and_comments()?;

            while !lexer.peek_rbracket() {
                let node = parse_node(lexer, allocator.clone())?;
                v.push(node);

                lexer.maybe_eat_whitespace_and_comments()?;

                if !lexer.peek_comma() {
                    break;
                }

                lexer.eat_comma().unwrap();
                lexer.maybe_eat_whitespace_and_comments()?;
            }

            lexer.eat_rbracket()?;

            Ok(Node::Array(v))
        }
        Token::String(string) => {
            let mut v = Vec::with_capacity_in(string.len(), allocator);
            v.extend(string.as_bytes());

            Ok(Node::String(v))
        }
        Token::Ident(ident) => {
            let ty = string_in(ident, allocator.clone());

            lexer.maybe_eat_whitespace_and_comments()?;

            let version = if lexer.peek_pound() {
                lexer.eat_pound()?;
                let v = lexer.eat_uint()?;

                Some(v)
            } else {
                None
            };

            lexer.maybe_eat_whitespace_and_comments()?;

            if lexer.peek_lbrace() {
                let mut v = DictionaryStructValues::with_capacity_in(DICTIONARY_LIKE_CAP, allocator.clone());

                lexer.eat_lbrace()?;

                lexer.maybe_eat_whitespace_and_comments()?;

                while !lexer.peek_rbrace() {
                    let key_str = lexer.eat_ident()?;
                    let key = string_in(key_str, allocator.clone());

                    lexer.maybe_eat_whitespace_and_comments()?;
                    lexer.eat_colon()?;
                    lexer.maybe_eat_whitespace_and_comments()?;

                    let node = parse_node(lexer, allocator.clone())?;
                    v.insert(key, node);

                    lexer.maybe_eat_whitespace_and_comments()?;

                    if !lexer.peek_comma() {
                        break;
                    }

                    lexer.eat_comma().unwrap();
                    lexer.maybe_eat_whitespace_and_comments()?;
                }

                lexer.eat_rbrace()?;

                Ok(Node::DictionaryStruct(ty, version, v))
            } else if lexer.peek_lparen() {
                if version.is_some() {
                    return Err(ParseError::UnexpectedToken {
                        line: lexer.line,
                        token: Token::LBracket,
                    });
                }

                let mut v = Vec::with_capacity_in(ARRAY_LIKE_CAP, allocator.clone());

                lexer.eat_lparen()?;

                lexer.maybe_eat_whitespace_and_comments()?;

                while !lexer.peek_rparen() {
                    let node = parse_node(lexer, allocator.clone())?;
                    v.push(node);

                    lexer.maybe_eat_whitespace_and_comments()?;

                    if !lexer.peek_comma() {
                        break;
                    }

                    lexer.eat_comma().unwrap();
                    lexer.maybe_eat_whitespace_and_comments()?;
                }

                lexer.eat_rparen()?;

                Ok(Node::TupleStruct(ty, v))
            } else {
                Ok(Node::UnitStruct(ty))
            }
        }
        Token::Int(int) => Ok(Node::Int(int)),
        Token::Uint(uint) => Ok(Node::Uint(uint)),
        Token::Float(float) => Ok(Node::Float(float)),
        Token::Boolean(boolean) => Ok(Node::Boolean(boolean)),
        Token::Null => Ok(Node::Null),
        Token::Eof => Err(ParseError::UnexpectedEof { line: lexer.line }),
        token => {
            // TODO(jt): This is a hack to create a token with a static lifetime, because I am too
            // stupid to tie the lifetime of ParseError to the lifetime of the input string and the
            // lexer.
            let err_token: Token<'static> = match token {
                Token::LBrace => Token::LBrace,
                Token::RBrace => Token::RBrace,
                Token::LParen => Token::LParen,
                Token::RParen => Token::RParen,
                Token::LBracket => Token::LBracket,
                Token::RBracket => Token::RBracket,
                Token::Pound => Token::Pound,
                Token::Colon => Token::Colon,
                Token::Comma => Token::Comma,
                Token::Whitespace => Token::Whitespace,
                Token::Ident(_) => Token::Ident("<unavailable>"),
                Token::String(_) => Token::String("<unavailable>"),
                Token::Int(i) => Token::Int(i),
                Token::Uint(u) => Token::Uint(u),
                Token::Float(f) => Token::Float(f),
                Token::Boolean(b) => Token::Boolean(b),
                Token::Null => Token::Null,
                Token::Eof => Token::Eof,
            };

            Err(ParseError::UnexpectedToken {
                line: lexer.line,
                token: err_token,
            })
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Pound,
    Colon,
    Comma,
    Whitespace,
    Ident(&'a str),
    String(&'a str),
    Int(i64),
    Uint(u64),
    Float(f64),
    Boolean(bool),
    Null,
    Eof,
}

struct Lexer<'a> {
    s: &'a str,
    line: u32,
}

impl Lexer<'_> {
    fn eat_token(&mut self) -> Result<Token<'_>, ParseError> {
        if self.maybe_eat_whitespace_and_comments()? {
            return Ok(Token::Whitespace);
        }

        if self.s.is_empty() {
            return Ok(Token::Eof);
        }

        let line = self.line;

        if self.s.starts_with('{') {
            self.s = &self.s[1..];
            return Ok(Token::LBrace);
        }

        if self.s.starts_with('}') {
            self.s = &self.s[1..];
            return Ok(Token::RBrace);
        }

        if self.s.starts_with('(') {
            self.s = &self.s[1..];
            return Ok(Token::LParen);
        }

        if self.s.starts_with(')') {
            self.s = &self.s[1..];
            return Ok(Token::RParen);
        }

        if self.s.starts_with('[') {
            self.s = &self.s[1..];
            return Ok(Token::LBracket);
        }

        if self.s.starts_with(']') {
            self.s = &self.s[1..];
            return Ok(Token::RBracket);
        }

        if self.s.starts_with('#') {
            self.s = &self.s[1..];
            return Ok(Token::Pound);
        }

        if self.s.starts_with(':') {
            self.s = &self.s[1..];
            return Ok(Token::Colon);
        }

        if self.s.starts_with(',') {
            self.s = &self.s[1..];
            return Ok(Token::Comma);
        }

        if self.s.starts_with("true") {
            self.s = &self.s[4..];
            return Ok(Token::Boolean(true));
        }

        if self.s.starts_with("false") {
            self.s = &self.s[5..];
            return Ok(Token::Boolean(false));
        }

        if self.s.starts_with("null") {
            self.s = &self.s[4..];
            return Ok(Token::Null);
        }

        if self.s.starts_with('"') {
            let string = self.eat_string()?;
            return Ok(Token::String(string));
        }

        if self.s.starts_with(is_ident_start_char) {
            let ident = self.eat_ident()?;
            return Ok(Token::Ident(ident));
        }

        // Float literals have EBNF:
        //
        // Float  ::= Sign? ( 'inf' | 'NaN' | Number )
        // Number ::= ( Digit+ |
        //              Digit+ '.' Digit* |
        //              Digit* '.' Digit+ ) Exp?
        // Exp    ::= [eE] Sign? Digit+
        // Sign   ::= [+-]
        // Digit  ::= [0-9]
        //
        // They are either inf, NaN, or can validly end with either a digit, or a period. It is
        // easier for us to determine something that follows, that for sure is not a part of the
        // number literal anymore.
        let end = self.seek_end_of_numeric_literal();
        if let Ok(int) = i64::from_str(&self.s[..end]) {
            self.s = &self.s[end..];
            Ok(Token::Int(int))
        } else if let Ok(uint) = u64::from_str(&self.s[..end]) {
            self.s = &self.s[end..];
            Ok(Token::Uint(uint))
        } else if let Ok(float) = f64::from_str(&self.s[..end]) {
            self.s = &self.s[end..];
            Ok(Token::Float(float))
        } else {
            let c = self.s.chars().next().unwrap();
            Err(ParseError::UnexpectedChar { line, c })
        }
    }

    fn eat_lbrace(&mut self) -> Result<(), ParseError> {
        self.eat_char('{')
    }

    fn eat_rbrace(&mut self) -> Result<(), ParseError> {
        self.eat_char('}')
    }

    fn eat_lparen(&mut self) -> Result<(), ParseError> {
        self.eat_char('(')
    }

    fn eat_rparen(&mut self) -> Result<(), ParseError> {
        self.eat_char(')')
    }

    fn eat_rbracket(&mut self) -> Result<(), ParseError> {
        self.eat_char(']')
    }

    fn eat_pound(&mut self) -> Result<(), ParseError> {
        self.eat_char('#')
    }

    fn eat_colon(&mut self) -> Result<(), ParseError> {
        self.eat_char(':')
    }

    fn eat_comma(&mut self) -> Result<(), ParseError> {
        self.eat_char(',')
    }

    fn eat_char(&mut self, c: char) -> Result<(), ParseError> {
        let eaten = self.s.chars().next();
        if eaten == Some(c) {
            let len = c.len_utf8();
            self.s = &self.s[len..];

            Ok(())
        } else {
            if let Some(eaten) = eaten {
                Err(ParseError::UnexpectedChar {
                    line: self.line,
                    c: eaten,
                })
            } else {
                Err(ParseError::UnexpectedEof { line: self.line })
            }
        }
    }

    fn eat_uint(&mut self) -> Result<u64, ParseError> {
        let end = self.seek_end_of_numeric_literal();
        if let Ok(uint) = u64::from_str(&self.s[..end]) {
            self.s = &self.s[end..];
            Ok(uint)
        } else {
            let c = self.s.chars().next().unwrap();
            Err(ParseError::UnexpectedChar { line: self.line, c })
        }
    }

    fn eat_string(&mut self) -> Result<&str, ParseError> {
        // TODO(yan): @Correctness This appears to eat multiline strings too
        // (which is ok), but it does not update the current line pointer.

        let line = self.line;
        let first_char = self.s.chars().next();
        if first_char == Some('"') {
            self.s = &self.s[1..];

            let mut end: Option<usize> = None;

            let mut prev = '"';
            for (i, c) in self.s.char_indices() {
                if c == '"' && prev != '\\' {
                    end = Some(i);
                    break;
                }

                prev = c;
            }

            if let Some(end) = end {
                let string = &self.s[..end];

                self.s = &self.s[end + 1..];

                Ok(string)
            } else {
                Err(ParseError::UnexpectedEof { line })
            }
        } else {
            if let Some(first_char) = first_char {
                Err(ParseError::UnexpectedChar { line, c: first_char })
            } else {
                Err(ParseError::UnexpectedEof { line })
            }
        }
    }

    fn eat_ident(&mut self) -> Result<&str, ParseError> {
        let line = self.line;

        let mut iter = self.s.char_indices();

        if let Some((i, c)) = iter.next() {
            if is_ident_start_char(c) {
                let mut end = i;

                for (i, c) in iter {
                    if !is_ident_char(c) {
                        break;
                    }
                    end = i;
                }

                let ident = &self.s[..=end];
                self.s = &self.s[end + 1..];

                Ok(ident)
            } else {
                Err(ParseError::UnexpectedChar { line, c })
            }
        } else {
            Err(ParseError::UnexpectedEof { line })
        }
    }

    fn maybe_eat_whitespace_and_comments(&mut self) -> Result<bool, ParseError> {
        let mut chars = self.s.chars();

        let mut advance_len = 0;
        let mut advance_lines = 0;

        while let Some(c) = chars.next() {
            if !c.is_whitespace() {
                // If we ate a forward slash. This either starts a comment, or
                // is a parse error. If it is a comment, we eat it whole, until
                // we find a newline. Then continue eating more whitespace.
                if c == '/' {
                    let c = chars.next();
                    // '/' has UTF8 length of one.
                    advance_len += 1;

                    if c != Some('/') {
                        let line = self.line + advance_lines;
                        if let Some(c) = c {
                            return Err(ParseError::UnexpectedChar { line, c });
                        } else {
                            return Err(ParseError::UnexpectedEof { line });
                        }
                    }

                    #[allow(clippy::while_let_on_iterator)]
                    while let Some(c) = chars.next() {
                        // Ok to eat, because newlines stops comments.
                        advance_len += c.len_utf8();

                        if c == '\n' {
                            advance_lines += 1;
                            break;
                        }
                    }
                } else {
                    break;
                }
            } else if c == '\n' {
                advance_lines += 1;
            }

            // Only eat if we didn't break.
            advance_len += c.len_utf8();
        }

        self.line += advance_lines;
        self.s = &self.s[advance_len..];

        Ok(advance_len > 0)
    }

    fn peek_lbrace(&self) -> bool {
        self.peek_char('{')
    }

    fn peek_rbrace(&self) -> bool {
        self.peek_char('}')
    }

    fn peek_lparen(&self) -> bool {
        self.peek_char('(')
    }

    fn peek_rparen(&self) -> bool {
        self.peek_char(')')
    }

    fn peek_rbracket(&self) -> bool {
        self.peek_char(']')
    }

    fn peek_comma(&self) -> bool {
        self.peek_char(',')
    }

    fn peek_pound(&self) -> bool {
        self.peek_char('#')
    }

    fn peek_char(&self, c: char) -> bool {
        self.s.starts_with(c)
    }

    fn seek_end_of_numeric_literal(&self) -> usize {
        // TODO(jt): @Correctness This breaks when we extend the syntax, like when we added
        // parens. We need reliably terminate ints and floats. One way would be to look for an int
        // end and a float end by skipping chars until we find one that isn't a valid int or float
        // char, try to parse both an int and a float, and pick the longer.
        self.s
            .find(|c: char| c.is_whitespace() || ['{', '}', '(', ')', '[', ']', ':', ',', '#'].contains(&c))
            .unwrap_or(self.s.len())
    }
}

fn string_in<A: Allocator>(str: &str, allocator: A) -> Vec<u8, A> {
    let mut buffer = Vec::with_capacity_in(str.len(), allocator);
    buffer.resize(str.len(), 0);
    buffer.clone_from_slice(str.as_bytes());
    buffer
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

    use super::*;

    fn s(str: &str) -> Vec<u8, Global> {
        string_in(str, Global)
    }

    #[test]
    fn test_parse_null() {
        let str = "null";
        let node = parse_objects_in(str, Global).unwrap();

        assert!(node == Node::Null);
    }

    #[test]
    fn test_parse_boolean() {
        let true_str = "true";
        let true_node = parse_objects_in(true_str, Global).unwrap();
        let false_str = "false";
        let false_node = parse_objects_in(false_str, Global).unwrap();

        assert!(true_node == Node::Boolean(true));
        assert!(false_node == Node::Boolean(false));
    }

    #[test]
    fn test_parse_int() {
        let str = "42";
        let node = parse_objects_in(str, Global).unwrap();

        assert!(node == Node::Int(42));
    }

    #[test]
    fn test_parse_float() {
        let str = "6.96";
        let node = parse_objects_in(str, Global).unwrap();

        assert!(node == Node::Float(6.96));
    }

    #[test]
    fn test_parse_unit_struct() {
        let str = "Peanut";
        let node = parse_objects_in(str, Global).unwrap();

        assert!(node == Node::UnitStruct(s("Peanut")));
    }

    #[test]
    fn test_parse_string() {
        let str = r#""peekaboo""#;
        let node = parse_objects_in(str, Global).unwrap();

        assert!(node == Node::String(s("peekaboo")));
    }

    #[test]
    fn test_parse_string_with_newline_escapes() {
        const EXPECT: &str = "peekaboo\nsays\nhello";
        let str = "\"peekaboo\nsays\nhello\"";
        let node = parse_objects_in(str, Global).unwrap();

        assert!(node == Node::String(s(EXPECT)));
    }

    #[test]
    fn test_parse_string_with_newlines() {
        const EXPECT: &str = "peekaboo
                     says
                     hello";
        let str = r#""peekaboo
                     says
                     hello""#;
        let node = parse_objects_in(str, Global).unwrap();

        assert!(node == Node::String(s(EXPECT)));
    }

    #[test]
    fn test_parse_string_with_newlines_and_ignored_comments() {
        const EXPECT: &str = "peekaboo
//                   says
                     hello";
        let str = "\"peekaboo
//                   says
                     hello\"";
        let node = parse_objects_in(str, Global).unwrap();

        assert!(node == Node::String(s(EXPECT)));
    }

    #[test]
    fn test_parse_array_empty() {
        let str = "[]";
        let node = parse_objects_in(str, Global).unwrap();

        assert!(node == Node::Array(Vec::new()));
    }

    #[test]
    fn test_parse_array_empty_whitespace() {
        let str = "[  ]";
        let node = parse_objects_in(str, Global).unwrap();

        assert!(node == Node::Array(Vec::new()));
    }

    #[test]
    fn test_parse_array_oneitem_compact() {
        let str = "[42]";
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::Array(Vec::from([Node::Int(42)])));
    }

    #[test]
    fn test_parse_array_oneitem_whitespace() {
        let str = "[  null  ]";
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::Array(Vec::from([Node::Null])));
    }

    #[test]
    fn test_parse_array_oneline_compact() {
        let str = r#"[null,false,true,42,6.96,"peekaboo",Peanut]"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::Array(Vec::from([
            Node::Null,
            Node::Boolean(false),
            Node::Boolean(true),
            Node::Int(42),
            Node::Float(6.96),
            Node::String(s("peekaboo")),
            Node::UnitStruct(s("Peanut")),
        ])));
    }

    #[test]
    fn test_parse_array_oneline_whitespace() {
        let str = r#"[null, false   , true   , 42, 6.96  , "peekaboo"  ,Peanut  ]"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::Array(Vec::from([
            Node::Null,
            Node::Boolean(false),
            Node::Boolean(true),
            Node::Int(42),
            Node::Float(6.96),
            Node::String(s("peekaboo")),
            Node::UnitStruct(s("Peanut")),
        ])));
    }

    #[test]
    fn test_parse_array_multiline() {
        let str = r#"[
            null,
            false,
            true,
            42,
            6.96,
            "peekaboo",
            Peanut
        ]"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::Array(Vec::from([
            Node::Null,
            Node::Boolean(false),
            Node::Boolean(true),
            Node::Int(42),
            Node::Float(6.96),
            Node::String(s("peekaboo")),
            Node::UnitStruct(s("Peanut")),
        ])));
    }

    #[test]
    fn test_parse_array_multiline_trailing_comma() {
        let str = r#"[
            null,
            false,
            true,
            42,
            6.96,
            "peekaboo",
            Peanut,
        ]"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::Array(Vec::from([
            Node::Null,
            Node::Boolean(false),
            Node::Boolean(true),
            Node::Int(42),
            Node::Float(6.96),
            Node::String(s("peekaboo")),
            Node::UnitStruct(s("Peanut")),
        ])));
    }

    #[test]
    fn test_parse_array_multiline_with_comments() {
        let str = r#"[
            null,
            false,
// This
    // Is
        // A
     // Multiline
 // Comment!
            true,
            // 42, There is no answer for us
            6.96, // Trailing commment, boy
            "peekaboo",
            Peanut,
        ]"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::Array(Vec::from([
            Node::Null,
            Node::Boolean(false),
            Node::Boolean(true),
            Node::Float(6.96),
            Node::String(s("peekaboo")),
            Node::UnitStruct(s("Peanut")),
        ])));
    }

    #[test]
    fn test_parse_tuple_empty() {
        let str = "()";
        let node = parse_objects_in(str, Global).unwrap();

        assert!(node == Node::Tuple(Vec::new()));
    }

    #[test]
    fn test_parse_tuple_empty_whitespace() {
        let str = "(  )";
        let node = parse_objects_in(str, Global).unwrap();

        assert!(node == Node::Tuple(Vec::new()));
    }

    #[test]
    fn test_parse_tuple_oneitem_compact() {
        let str = "(42)";
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::Tuple(Vec::from([Node::Int(42)])));
    }

    #[test]
    fn test_parse_tuple_oneitem_whitespace() {
        let str = "(  null  )";
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::Tuple(Vec::from([Node::Null])));
    }

    #[test]
    fn test_parse_tuple_oneline_compact() {
        let str = r#"(null,false,true,42,6.96,"peekaboo",Peanut)"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::Tuple(Vec::from([
            Node::Null,
            Node::Boolean(false),
            Node::Boolean(true),
            Node::Int(42),
            Node::Float(6.96),
            Node::String(s("peekaboo")),
            Node::UnitStruct(s("Peanut")),
        ])));
    }

    #[test]
    fn test_parse_tuple_oneline_whitespace() {
        let str = r#"(null, false   , true   , 42, 6.96  , "peekaboo"  ,Peanut  )"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::Tuple(Vec::from([
            Node::Null,
            Node::Boolean(false),
            Node::Boolean(true),
            Node::Int(42),
            Node::Float(6.96),
            Node::String(s("peekaboo")),
            Node::UnitStruct(s("Peanut")),
        ])));
    }

    #[test]
    fn test_parse_tuple_multiline() {
        let str = r#"(
            null,
            false,
            true,
            42,
            6.96,
            "peekaboo",
            Peanut
        )"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::Tuple(Vec::from([
            Node::Null,
            Node::Boolean(false),
            Node::Boolean(true),
            Node::Int(42),
            Node::Float(6.96),
            Node::String(s("peekaboo")),
            Node::UnitStruct(s("Peanut")),
        ])));
    }

    #[test]
    fn test_parse_tuple_multiline_trailing_comma() {
        let str = r#"(
            null,
            false,
            true,
            42,
            6.96,
            "peekaboo",
            Peanut,
        )"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::Tuple(Vec::from([
            Node::Null,
            Node::Boolean(false),
            Node::Boolean(true),
            Node::Int(42),
            Node::Float(6.96),
            Node::String(s("peekaboo")),
            Node::UnitStruct(s("Peanut")),
        ])));
    }

    #[test]
    fn test_parse_tuple_multiline_with_comments() {
        let str = r#"(
            null,
            false,
// This
    // Is
        // A
     // Multiline
 // Comment!
            true,
            // 42, There is no answer for us
            6.96, // Trailing commment, boy
            "peekaboo",
            Peanut,
        )"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::Tuple(Vec::from([
            Node::Null,
            Node::Boolean(false),
            Node::Boolean(true),
            Node::Float(6.96),
            Node::String(s("peekaboo")),
            Node::UnitStruct(s("Peanut")),
        ])));
    }

    #[test]
    fn test_parse_tuple_struct_empty_compact() {
        let str = "Chilli()";
        let node = parse_objects_in(str, Global).unwrap();

        assert!(node == Node::TupleStruct(s("Chilli"), Vec::new()));
    }

    #[test]
    fn test_parse_tuple_struct_empty_whitespace() {
        let str = "Chilli ()";
        let node = parse_objects_in(str, Global).unwrap();

        assert!(node == Node::TupleStruct(s("Chilli"), Vec::new()));
    }

    #[test]
    fn test_parse_tuple_struct_oneitem_compact() {
        let str = r#"Chilli("Beans")"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::TupleStruct(s("Chilli"), Vec::from([
            Node::String(s("Beans")),
        ])));
    }

    #[test]
    fn test_parse_tuple_struct_oneitem_whitespace() {
        let str = r#"Chilli( "Beans" )"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::TupleStruct(s("Chilli"), Vec::from([
            Node::String(s("Beans")),
        ])));
    }

    #[test]
    fn test_parse_tuple_struct_oneline_compact() {
        let str = r#"Chilli("Beans",Peanut,butter,42)"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::TupleStruct(s("Chilli"), Vec::from([
            Node::String(s("Beans")),
            Node::UnitStruct(s("Peanut")),
            Node::UnitStruct(s("butter")),
            Node::Int(42),
        ])));
    }

    #[test]
    fn test_parse_tuple_struct_oneline_whitespace() {
        let str = r#"Chilli("Beans", Peanut     ,butter , 42   )"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::TupleStruct(s("Chilli"), Vec::from([
            Node::String(s("Beans")),
            Node::UnitStruct(s("Peanut")),
            Node::UnitStruct(s("butter")),
            Node::Int(42),
        ])));
    }

    #[test]
    fn test_parse_tuple_struct_multiline() {
        let str = r#"Chilli(
            "Beans",
             Peanut,
             butter,
             42
        )"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::TupleStruct(s("Chilli"), Vec::from([
            Node::String(s("Beans")),
            Node::UnitStruct(s("Peanut")),
            Node::UnitStruct(s("butter")),
            Node::Int(42),
        ])));
    }

    #[test]
    fn test_parse_tuple_struct_multiline_trailing_comma() {
        let str = r#"Chilli(
            "Beans",
             Peanut,
             butter,
             42,
        )"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::TupleStruct(s("Chilli"), Vec::from([
            Node::String(s("Beans")),
            Node::UnitStruct(s("Peanut")),
            Node::UnitStruct(s("butter")),
            Node::Int(42),
        ])));
    }

    #[test]
    fn test_parse_dictionary_empty() {
        let str = "{}";
        let node = parse_objects_in(str, Global).unwrap();

        assert!(node == Node::Dictionary(DictionaryValues::new()));
    }

    #[test]
    fn test_parse_dictionary_empty_whitespace() {
        let str = "{  }";
        let node = parse_objects_in(str, Global).unwrap();

        assert!(node == Node::Dictionary(DictionaryValues::new()));
    }

    #[test]
    fn test_parse_dictionary_oneitem_compact() {
        let str = r#"{"key":42}"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::Dictionary(DictionaryValues::from([
            (s("key"), Node::Int(42)),
        ])));
    }

    #[test]
    fn test_parse_dictionary_oneitem_whitespace() {
        let str = r#"{  "key" :   42 }"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::Dictionary(DictionaryValues::from([
            (s("key"), Node::Int(42)),
        ])));
    }

    #[test]
    fn test_parse_dictionary_oneline_compact() {
        let str = r#"{"key":42,"another key":"another node","yak":Peanut}"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::Dictionary(DictionaryValues::from([
            (s("key"), Node::Int(42)),
            (s("another key"), Node::String(s("another node"))),
            (s("yak"), Node::UnitStruct(s("Peanut"))),
        ])));
    }

    #[test]
    fn test_parse_dictionary_multiline() {
        let str = r#"{
            "key 1": null,
            "key 2": false,
            "key 3": true,
            "key 4": 42,
            "key 5": 6.96,
            "key 6": "peekaboo",
            "key 7": Peanut
        }"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::Dictionary(DictionaryValues::from([
            (s("key 1"), Node::Null),
            (s("key 2"), Node::Boolean(false)),
            (s("key 3"), Node::Boolean(true)),
            (s("key 4"), Node::Int(42)),
            (s("key 5"), Node::Float(6.96)),
            (s("key 6"), Node::String(s("peekaboo"))),
            (s("key 7"), Node::UnitStruct(s("Peanut"))),
        ])));
    }

    #[test]
    fn test_parse_dictionary_multiline_trailing_comma() {
        let str = r#"{
            "key 1": null,
            "key 2": false,
            "key 3": true,
            "key 4": 42,
            "key 5": 6.96,
            "key 6": "peekaboo",
            "key 7": Peanut,
        }"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::Dictionary(DictionaryValues::from([
            (s("key 1"), Node::Null),
            (s("key 2"), Node::Boolean(false)),
            (s("key 3"), Node::Boolean(true)),
            (s("key 4"), Node::Int(42)),
            (s("key 5"), Node::Float(6.96)),
            (s("key 6"), Node::String(s("peekaboo"))),
            (s("key 7"), Node::UnitStruct(s("Peanut"))),
        ])));
    }

    #[test]
    fn test_parse_dictionary_multiline_with_comments() {
        let str = r#"{
            "key 1": null, // Hi!
            "key 2": false,
            "key 3": true,

// A
// few
// comments

            // "key 4": 42, No answer for us
            "key 5": 6.96,
            "key 6": "peekaboo",
            "key 7": Peanut,
        }"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::Dictionary(DictionaryValues::from([
            (s("key 1"), Node::Null),
            (s("key 2"), Node::Boolean(false)),
            (s("key 3"), Node::Boolean(true)),
            (s("key 5"), Node::Float(6.96)),
            (s("key 6"), Node::String(s("peekaboo"))),
            (s("key 7"), Node::UnitStruct(s("Peanut"))),
        ])));
    }

    #[test]
    fn test_parse_dictionary_struct_empty_compact() {
        let str = "V60{}";
        let node = parse_objects_in(str, Global).unwrap();

        assert!(node == Node::DictionaryStruct(s("V60"), None, DictionaryStructValues::new()));
    }

    #[test]
    fn test_parse_dictionary_struct_empty_whitespace() {
        let str = "V60 {}";
        let node = parse_objects_in(str, Global).unwrap();

        assert!(node == Node::DictionaryStruct(s("V60"), None, DictionaryStructValues::new()));
    }

    #[test]
    fn test_parse_dictionary_struct_with_version_empty_compact() {
        let str = "V60#42{}";
        let node = parse_objects_in(str, Global).unwrap();

        assert!(node == Node::DictionaryStruct(s("V60"), Some(42), DictionaryStructValues::new()));
    }

    #[test]
    fn test_parse_dictionary_struct_with_version_empty_whitespace() {
        let str = "V60 #42 {}";
        let node = parse_objects_in(str, Global).unwrap();

        assert!(
            node == Node::DictionaryStruct(s("V60"), Some(42), DictionaryStructValues::new()),
            "{node:?}"
        );
    }

    #[test]
    fn test_parse_dictionary_struct_oneitem_compact() {
        let str = r#"V60 {ingredient_1:"Beans"}"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::DictionaryStruct(s("V60"), None, DictionaryStructValues::from([
            (s("ingredient_1"), Node::String(s("Beans"))),
        ])));
    }

    #[test]
    fn test_parse_dictionary_struct_with_version_oneitem_compact() {
        let str = r#"V60 #69 {ingredient_1:"Beans"}"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::DictionaryStruct(s("V60"), Some(69), DictionaryStructValues::from([
            (s("ingredient_1"), Node::String(s("Beans"))),
        ])));
    }

    #[test]
    fn test_parse_dictionary_struct_oneitem_whitespace() {
        let str = r#"V60 { ingredient_1 : "Beans" }"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::DictionaryStruct(s("V60"), None, DictionaryStructValues::from([
            (s("ingredient_1"), Node::String(s("Beans"))),
        ])));
    }

    #[test]
    fn test_parse_dictionary_struct_oneline_compact() {
        let str = r#"V60 {ingredient_1:"Beans",ingredient_2:Water}"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::DictionaryStruct(s("V60"), None, DictionaryStructValues::from([
            (s("ingredient_1"), Node::String(s("Beans"))),
            (s("ingredient_2"), Node::UnitStruct(s("Water"))),
        ])));
    }

    #[test]
    fn test_parse_dictionary_struct_oneline_whitespace() {
        let str = r#"V60 { ingredient_1:    "Beans" ,   ingredient_2:Water   }"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::DictionaryStruct(s("V60"), None, DictionaryStructValues::from([
            (s("ingredient_1"), Node::String(s("Beans"))),
            (s("ingredient_2"), Node::UnitStruct(s("Water"))),
        ])));
    }

    #[test]
    fn test_parse_dictionary_struct_multiline() {
        let str = r#"V60 {
          ingredient_1: "Beans",
          ingredient_2: Water
        }"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::DictionaryStruct(s("V60"), None, DictionaryStructValues::from([
            (s("ingredient_1"), Node::String(s("Beans"))),
            (s("ingredient_2"), Node::UnitStruct(s("Water"))),
        ])));
    }

    #[test]
    fn test_parse_dictionary_struct_multiline_trailing_comma() {
        let str = r#"V60 {
          ingredient_1: "Beans",
          ingredient_2: Water,
        }"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::DictionaryStruct(s("V60"), None, DictionaryStructValues::from([
            (s("ingredient_1"), Node::String(s("Beans"))),
            (s("ingredient_2"), Node::UnitStruct(s("Water"))),
        ])));
    }

    #[test]
    fn test_parse_nested() {
        let str = r#"{
            "a": [1, 2, 3],
            "b": { "a": null },
            "c": [{ "a": null }],
        }"#;
        let node = parse_objects_in(str, Global).unwrap();

        #[rustfmt::skip]
        assert!(node == Node::Dictionary(DictionaryValues::from([
            (s("a"), Node::Array(Vec::from([
                Node::Int(1),
                Node::Int(2),
                Node::Int(3),
            ]))),
            (s("b"), Node::Dictionary(DictionaryValues::from([
                (s("a"), Node::Null),
            ]))),
            (s("c"), Node::Array(Vec::from([
                Node::Dictionary(DictionaryValues::from([
                    (s("a"), Node::Null),
                ])),
            ]))),
        ])));
    }
}
