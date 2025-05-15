use std::fmt::{self, Arguments, Display, Write as _};
use std::fs;
use std::io::Write as _;

use anyhow::Result;

use lc3_format::{TokenKind, Tokenizer};

fn main() -> Result<()> {
    let input = include_str!("../example.asm");
    // let input = include_str!("/home/darcy/code/lasim/examples/char_count.asm");

    let mut output = fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open("output.asm")
        .unwrap();

    let formatter = LineFormatter::new(input);

    let mut lines = Vec::new();
    for line in formatter {
        let line = line?;
        lines.push(line);
    }

    // Indent a comment-line if next line is indented
    for i in 0..lines.len() - 1 {
        let line = &lines[i];
        let next = &lines[i + 1];

        if line.payload.is_empty() && !line.comment.is_empty() {
            if next.indent {
                let line = &mut lines[i];
                line.indent = true;
            }
        }
    }

    for line in lines {
        match (!line.payload.is_empty(), !line.comment.is_empty()) {
            (true, true) => {
                if line.indent {
                    write!(&mut output, "    ")?;
                }
                writeln!(&mut output, "{} {}", line.payload, line.comment)?;
            }
            (true, false) => {
                if line.indent {
                    write!(&mut output, "    ")?;
                }
                writeln!(&mut output, "{}", line.payload)?;
            }
            (false, true) => {
                if line.indent {
                    write!(&mut output, "    ")?;
                }
                writeln!(&mut output, "{}", line.comment)?;
            }
            (false, false) => {
                writeln!(&mut output)?;
            }
        }
    }

    Ok(())
}

#[derive(Debug)]
struct LineParts {
    indent: bool,
    payload: String,
    comment: String,
}

#[derive(Clone, Copy, Debug, Default)]
enum PreviousToken {
    #[default]
    None,
    Comma,
    Directive,
    LabelDeclaration,
    Operand,
    // `Mnemonic`, `Unknown`
    Other,
}

struct Payload {
    string: String,
    previous: PreviousToken,
    indent: Option<bool>,
}

impl Payload {
    pub fn new() -> Self {
        Self {
            string: String::new(),
            previous: PreviousToken::default(),
            indent: None,
        }
    }
    pub fn finish(self) -> (bool, String) {
        (self.indent.unwrap_or(false), self.string)
    }

    pub fn write_token(&mut self, token_kind: TokenKind, token_str: &str) {
        match token_kind {
            TokenKind::Linebreak => {
                unreachable!();
            }
            TokenKind::Comment => {
                unreachable!();
            }

            TokenKind::Comma => {}
            TokenKind::Colon => {}

            TokenKind::Dir => {
                self.ensure_space();
                self.write_fmt(format_args!("{}", Lowercase::from(token_str)));
                self.previous = PreviousToken::Directive;
                self.set_indent(false);
            }

            TokenKind::Mnemonic => {
                self.ensure_space();
                self.write_fmt(format_args!("{}", Lowercase::from(token_str)));
                self.previous = PreviousToken::Other;
                self.set_indent(true);
            }

            TokenKind::Label => match self.previous {
                PreviousToken::None => {
                    self.string += token_str;
                    self.previous = PreviousToken::LabelDeclaration;
                    self.set_indent(false);
                }
                _ => {
                    self.ensure_comma();
                    self.string += token_str;
                    self.previous = PreviousToken::Operand;
                    self.set_indent(true);
                }
            },

            TokenKind::Register => {
                self.ensure_comma();
                self.write_fmt(format_args!("{}", Lowercase::from(token_str)));
                self.previous = PreviousToken::Operand;
                self.set_indent(true);
            }

            TokenKind::IntegerLiteral => {
                self.ensure_comma();
                // TODO: Standardize prefix form
                self.write_fmt(format_args!("{}", Lowercase::from(token_str)));
                self.previous = PreviousToken::Operand;
                self.set_indent(true);
            }

            TokenKind::StringLiteral => {
                self.ensure_space();
                self.string += token_str;
                // TODO(check): Is this right?
                self.previous = PreviousToken::Operand;
                self.set_indent(true);
            }

            TokenKind::Unknown => {
                self.string += token_str;
                self.previous = PreviousToken::Other;
            }
        }
    }

    fn set_indent(&mut self, indent: bool) {
        if self.indent.is_none() {
            self.indent = Some(indent);
        }
    }

    fn write_fmt(&mut self, fmt: Arguments<'_>) {
        if self.string.write_fmt(fmt).is_err() {
            unreachable!();
        }
    }

    fn ensure_comma(&mut self) {
        match self.previous {
            PreviousToken::None | PreviousToken::Comma => {}
            PreviousToken::Operand => {
                self.string += ", ";
                self.previous = PreviousToken::Comma;
            }
            _ => {
                self.string += " ";
            }
        }
    }

    fn ensure_space(&mut self) {
        match self.previous {
            PreviousToken::None => {}
            _ => {
                self.string += " ";
            }
        }
    }
}

struct LineFormatter<'a> {
    input: &'static str,
    tokens: Tokenizer<'a>,
}

impl<'a> LineFormatter<'a> {
    pub fn new(input: &'static str) -> Self {
        let tokens = Tokenizer::new(input);
        Self { input, tokens }
    }
}

impl<'a> Iterator for LineFormatter<'a> {
    type Item = Result<LineParts>;

    fn next(&mut self) -> Option<Result<LineParts>> {
        let mut payload = Payload::new();
        let mut comment: Option<&str> = None;

        for i in 0.. {
            let Some(token) = self.tokens.next() else {
                if i == 0 {
                    return None;
                }
                break;
            };

            let token = match token {
                Ok(token) => token,
                Err(error) => return Some(Err(error)),
            };

            let token_str = &self.input[token.span.offs()..token.span.end()];

            match token.kind {
                TokenKind::Linebreak => break,

                TokenKind::Comment => {
                    assert!(comment.is_none(), "line with multiple comments");
                    comment = Some(token_str);
                }

                _ => {
                    payload.write_token(token.kind, token_str);
                }
            }
        }

        let (indent, payload) = payload.finish();

        assert!(
            !indent || !payload.is_empty(),
            "comment-line should not be indented yet",
        );

        Some(Ok(LineParts {
            indent,
            payload,
            comment: comment.unwrap_or("").to_string(),
        }))
    }
}

struct Lowercase<T>
where
    T: AsRef<str>,
{
    string: T,
}

impl<T> From<T> for Lowercase<T>
where
    T: AsRef<str>,
{
    fn from(string: T) -> Self {
        Self { string }
    }
}

impl<T> Display for Lowercase<T>
where
    T: AsRef<str>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for ch in self.string.as_ref().chars() {
            write!(f, "{}", ch.to_lowercase())?;
        }
        Ok(())
    }
}
