use std::ops::Range;

use anyhow::Result;

struct CharIter<'a> {
    string: &'a str,
    index: usize,
}

impl<'a> CharIter<'a> {
    pub fn new(string: &'a str) -> Self {
        Self { string, index: 0 }
    }

    pub fn peek(&mut self) -> Option<char> {
        let remaining = &self.string[self.index..];
        let ch = remaining.chars().next()?;
        Some(ch)
    }

    pub fn get(&self, range: Range<usize>) -> Option<&str> {
        self.string.get(range)
    }

    pub fn index(&self) -> usize {
        self.index
    }
}

impl<'a> Iterator for CharIter<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let remaining = &self.string[self.index..];
        let ch = remaining.chars().next()?;
        self.index += ch.len_utf8();
        Some(ch)
    }
}

pub struct Tokenizer<'a> {
    chars: CharIter<'a>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(file: &'a str) -> Self {
        Self {
            chars: CharIter::new(file),
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let (start, ch) = loop {
            let start = self.chars.index();
            let ch = self.chars.next()?;
            if ch != ' ' {
                break (start, ch);
            }
        };

        let mut end = start;

        let kind = match ch {
            ' ' => unreachable!(),

            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,
            '\n' => TokenKind::Linebreak,

            ';' => {
                while let Some(ch) = self.chars.peek() {
                    if ch == '\n' {
                        break;
                    }
                    self.chars.next();
                }
                end = self.chars.index();
                TokenKind::Comment
            }

            '"' => {
                while let Some(ch) = self.chars.next() {
                    if ch == '"' {
                        break;
                    }
                }
                end = self.chars.index();
                TokenKind::StringLiteral
            }

            _ => {
                while let Some(ch) = self.chars.peek() {
                    if matches!(ch, ',' | ':' | ';') || ch.is_whitespace() {
                        break;
                    }
                    self.chars.next();
                }
                end = self.chars.index();
                resolve_token_kind(self.chars.get(start..end).unwrap())
            }
        };

        let span = Span::from(start..end);

        Some(Ok(Token { span, kind }))
    }
}

fn resolve_token_kind(string: &str) -> TokenKind {
    if is_directive(string) {
        return TokenKind::Dir;
    }
    if is_mnemonic(string) {
        return TokenKind::Mnemonic;
    }
    if is_register(string) {
        return TokenKind::Register;
    }
    if is_integer_literal(string) {
        return TokenKind::IntegerLiteral;
    }
    if is_label(string) {
        return TokenKind::Label;
    }
    TokenKind::Unknown
}

fn is_directive(string: &str) -> bool {
    string.chars().next().is_some_and(|ch| ch == '.')
}

fn is_mnemonic(string: &str) -> bool {
    const MNEMONICS: &[&str] = &[
        "add", "and", "br", "brnzp", "brnz", "brzp", "brnp", "brn", "brz", "brp", "jmp", "jsr",
        "jsrr", "ld", "ldi", "ldr", "lea", "not", "ret", "rti", "st", "sti", "str", "pop", "push",
        "call", "rets", "trap", "getc", "out", "puts", "in", "putsp", "halt", "putn", "reg",
    ];
    MNEMONICS
        .iter()
        .any(|mnemonic| string.eq_ignore_ascii_case(mnemonic))
}

fn is_register(string: &str) -> bool {
    let mut chars = string.chars();
    chars.next().is_some_and(|ch| matches!(ch, 'r' | 'R'))
        && chars.next().is_some_and(|ch| matches!(ch, '0'..='9'))
        && chars.next().is_none()
}

fn is_integer_literal(string: &str) -> bool {
    let mut chars = string.chars();

    // Next non-zero character
    let ch = loop {
        match chars.next() {
            None => return false,
            Some('0') => continue,
            Some(ch) => break ch,
        }
    };

    if matches!(ch, '#' | '0'..='9') {
        return true;
    }

    let radix = match ch {
        'x' | 'X' => Radix::Hex,
        'o' | 'O' => Radix::Octal,
        _ => return false,
    };

    // Discern `xa` (integer) and `xaz` (label)
    chars.all(|ch| match (radix, ch) {
        (Radix::Hex, '0'..='9' | 'a'..='z' | 'A'..='Z') => true,
        (Radix::Octal, '0'..='8') => true,
        _ => false,
    })
}

fn is_label(string: &str) -> bool {
    let mut chars = string.chars();
    matches!(chars.next(), Some('a'..='z' | 'A'..='Z' | '_'))
        && chars.all(|ch| matches!(ch, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'))
}

#[derive(Clone, Copy)]
enum Radix {
    Hex,
    Octal,
    // Decimal already checked
}

#[derive(Clone, Copy, Debug)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

#[derive(Clone, Copy, Debug)]
pub enum TokenKind {
    Dir,
    Mnemonic,
    Label,
    Register,
    IntegerLiteral,

    StringLiteral,
    Comment,

    Comma,
    Colon,
    Linebreak,

    Unknown,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Span {
    offs: SrcOffset,
    len: usize,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct SrcOffset(pub usize);

impl Span {
    pub fn new(offs: SrcOffset, len: usize) -> Self {
        Span { offs, len }
    }

    /// Non-source span
    pub fn dummy() -> Self {
        Span {
            offs: SrcOffset(0),
            len: 0,
        }
    }

    /// Returns a range that can be used to index the source
    pub fn as_range(&self) -> Range<usize> {
        self.offs()..self.end()
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn offs(&self) -> usize {
        self.offs.0
    }

    pub fn end(&self) -> usize {
        self.offs.0 + self.len
    }
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Span {
            offs: SrcOffset(value.start),
            len: value.end - value.start,
        }
    }
}

impl From<Span> for Range<usize> {
    fn from(value: Span) -> Self {
        value.offs()..value.end()
    }
}
