use std::fmt::{Display, Formatter, Result};

#[derive(Clone, Debug)]
pub enum EvalError {
    /// Previous value of witness column not known when trying to derive a value in the next row.
    PreviousValueUnknown(String),
    Generic(String),
    Multiple(Vec<EvalError>),
}

pub fn combine(left: EvalError, right: EvalError) -> EvalError {
    match (left, right) {
        (EvalError::Multiple(l), EvalError::Multiple(r)) => {
            EvalError::Multiple(l.into_iter().chain(r).collect())
        }
        (m @ EvalError::Multiple(_), other) | (other, m @ EvalError::Multiple(_)) => {
            combine(m, EvalError::Multiple(vec![other]))
        }
        (l, r) => EvalError::Multiple(vec![l, r]),
    }
}

impl From<String> for EvalError {
    fn from(value: String) -> Self {
        EvalError::Generic(value)
    }
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            EvalError::Generic(reason) => write!(f, "{reason}"),
            EvalError::PreviousValueUnknown(names) => write!(
                f,
                "Previous value of the following column(s) is not (yet) known: {names}.",
            ),
            EvalError::Multiple(errors) => {
                let (previous_unknown, mut others) = errors.iter().fold(
                    (vec![], vec![]),
                    |(mut previous_unknown, mut others), err| {
                        if let EvalError::PreviousValueUnknown(n) = err {
                            previous_unknown.push(n.clone());
                        } else {
                            others.push(format!("{err}"));
                        }
                        (previous_unknown, others)
                    },
                );
                if !previous_unknown.is_empty() {
                    others.push(format!(
                        "{}",
                        EvalError::PreviousValueUnknown(previous_unknown.join(", "))
                    ));
                }
                write!(f, "{}", others.join("\n"))
            }
        }
    }
}
