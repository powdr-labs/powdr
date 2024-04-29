//! Utils used with different lalrpop parsers

#![deny(clippy::print_stdout)]

pub mod lines;

#[derive(Debug)]
pub struct ParseError<'a> {
    start: usize,
    end: usize,
    file_name: String,
    contents: &'a str,
    message: String,
}

impl<'a> ParseError<'a> {
    pub fn output_to_stderr(&self) {
        use codespan_reporting::diagnostic::{Diagnostic, Label};
        use codespan_reporting::files::SimpleFiles;
        use codespan_reporting::term;
        use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

        let config = term::Config::default();
        let mut files = SimpleFiles::new();
        let file_id = files.add(&self.file_name, self.contents);
        let diagnostic = Diagnostic::error()
            .with_message(&self.message)
            .with_labels(vec![Label::primary(file_id, self.start..self.end)]);
        let mut writer = StandardStream::stderr(ColorChoice::Always);
        term::emit(&mut writer, &config, &files, &diagnostic).unwrap()
    }
}

pub fn handle_parse_error<'a>(
    err: lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token, String>,
    file_name: Option<&str>,
    input: &'a str,
) -> ParseError<'a> {
    let (&start, &end) = match &err {
        lalrpop_util::ParseError::InvalidToken { location } => (location, location),
        lalrpop_util::ParseError::UnrecognizedEOF {
            location,
            expected: _,
        } => (location, location),
        lalrpop_util::ParseError::UnrecognizedToken {
            token: (start, _, end),
            expected: _,
        } => (start, end),
        lalrpop_util::ParseError::ExtraToken {
            token: (start, _, end),
        } => (start, end),
        lalrpop_util::ParseError::User { error: _ } => (&0, &0),
    };
    ParseError {
        start,
        end,
        file_name: file_name.unwrap_or("input").to_string(),
        contents: input,
        message: format!("{err}"),
    }
}

/// Convenience trait that outputs parser errors to stderr and panics.
/// Should be used mostly in tests.
pub trait UnwrapErrToStderr {
    type Inner;
    fn unwrap_err_to_stderr(self) -> Self::Inner;
}

impl<'a, T> UnwrapErrToStderr for Result<T, ParseError<'a>> {
    type Inner = T;

    fn unwrap_err_to_stderr(self) -> Self::Inner {
        match self {
            Ok(r) => r,
            Err(err) => {
                err.output_to_stderr();
                panic!("Parse error.");
            }
        }
    }
}
