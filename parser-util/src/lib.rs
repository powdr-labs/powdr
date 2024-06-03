//! Utils used with different lalrpop parsers

#![deny(clippy::print_stdout)]

use std::sync::Arc;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[derive(
    Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, JsonSchema,
)]
pub struct SourceRef {
    pub file_name: Option<Arc<str>>,
    pub file_contents: Option<Arc<str>>,
    pub start: usize,
    pub end: usize,
}

impl SourceRef {
    pub fn unknown() -> Self {
        Default::default()
    }
}

#[derive(Debug)]
pub struct Error {
    source_ref: SourceRef,
    message: String,
}

impl Error {
    pub fn output_to_stderr(&self) {
        use codespan_reporting::diagnostic::{Diagnostic, Label};
        use codespan_reporting::files::SimpleFiles;
        use codespan_reporting::term;
        use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

        let config = term::Config::default();
        let mut files = SimpleFiles::new();
        let file_name = self.source_ref.file_name.as_deref().unwrap_or("input");
        let contents = self.source_ref.file_contents.as_deref().unwrap_or_default();
        let file_id = files.add(file_name, contents);
        let diagnostic = Diagnostic::error()
            .with_message(&self.message)
            .with_labels(vec![Label::primary(
                file_id,
                self.source_ref.start..self.source_ref.end,
            )]);
        let mut writer = StandardStream::stderr(ColorChoice::Always);
        term::emit(&mut writer, &config, &files, &diagnostic).unwrap()
    }
}

pub fn handle_parse_error(
    err: lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token, String>,
    file_name: Option<&str>,
    input: &str,
) -> Error {
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
    Error {
        source_ref: SourceRef {
            start,
            end,
            file_name: file_name.map(Into::into),
            file_contents: Some(input.into()),
        },
        message: err.to_string(),
    }
}

/// Convenience trait that outputs parser errors to stderr and panics.
/// Should be used mostly in tests.
pub trait UnwrapErrToStderr {
    type Inner;
    fn unwrap_err_to_stderr(self) -> Self::Inner;
}

impl<T> UnwrapErrToStderr for Result<T, Error> {
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
