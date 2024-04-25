use powdr_number::BigInt;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use std::{
    iter::{empty, once},
    str::FromStr,
};

use super::{asm::SymbolPath, visitor::Children};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema)]
pub enum Pattern {
    CatchAll, // "_", matches a single value
    Ellipsis, // "..", matches a series of values, only valid inside array patterns
    #[schemars(skip)]
    Number(BigInt),
    String(String),
    Tuple(Vec<Pattern>),
    Array(Vec<Pattern>),
    // A pattern that binds a variable. Variable references are parsed as
    // Enum and are then re-mapped to Variable if they do not reference
    // an enum variant.
    Variable(String),
    Enum(SymbolPath, Option<Vec<Pattern>>),
}

impl Pattern {
    /// Returns an iterator over all variables in this pattern.
    pub fn variables(&self) -> Box<dyn Iterator<Item = &String> + '_> {
        match self {
            Pattern::Variable(v) => Box::new(once(v)),
            _ => Box::new(self.children().flat_map(|p| p.variables())),
        }
    }

    /// Return true if the pattern is irrefutable, i.e. matches all possible values of its type.
    pub fn is_irrefutable(&self) -> bool {
        match self {
            Pattern::Ellipsis => unreachable!(),
            Pattern::CatchAll | Pattern::Variable(_) => true,
            Pattern::Number(_) | Pattern::String(_) | Pattern::Enum(_, _) => false,
            Pattern::Array(items) => {
                // Only "[..]"" is irrefutable
                items == &vec![Pattern::Ellipsis]
            }
            Pattern::Tuple(p) => p.iter().all(|p| p.is_irrefutable()),
        }
    }

    // Specialize a pattern based on a "constructor" pattern passed as a parameter.
    // Based on https://doc.rust-lang.org/nightly/nightly-rustc/rustc_pattern_analysis/usefulness/index.html#specialization.
    pub fn specialize(&self, constructor: &Self) -> Option<Vec<Self>> {
        match (constructor, self) {
            //(Pattern::CatchAll, _) => Some([].to_vec()),
            (Pattern::CatchAll, Pattern::CatchAll) => Some([].to_vec()),

            (Pattern::Number(y), Pattern::Number(x)) => {
                if y == x {
                    Some([].to_vec())
                } else {
                    None
                }
            }
            (Pattern::String(r), Pattern::String(s)) => {
                if r == s {
                    Some([].to_vec())
                } else {
                    None
                }
            }

            (Pattern::Tuple(cons_patterns), Pattern::Tuple(patterns))
                if cons_patterns.len() == patterns.len() =>
            {
                Some(patterns.to_vec())
            }
            (Pattern::Array(cons_patterns), Pattern::Array(patterns)) => {
                let mut specialized = Vec::new();

                let mut pre_ellipsis = Vec::new();
                let mut post_ellipsis = Vec::new();
                let mut seen_ellipsis = false;

                for pat in patterns {
                    match *pat {
                        Pattern::Ellipsis => seen_ellipsis = true,
                        _ if !seen_ellipsis => pre_ellipsis.push(pat),
                        _ if seen_ellipsis => post_ellipsis.push(pat),
                        _ => {}
                    }
                }

                let mut cons_iter = cons_patterns.iter().peekable();

                for pat in pre_ellipsis.iter() {
                    if cons_iter.next().is_some() {
                        specialized.push((*pat).clone());
                    }
                }

                let skip_count = cons_patterns.len() - pre_ellipsis.len() - post_ellipsis.len();
                for _ in 0..skip_count {
                    specialized.push(Pattern::CatchAll);
                    cons_iter.next();
                }

                for pat in post_ellipsis {
                    if cons_iter.next().is_some() {
                        specialized.push(pat.clone());
                    }
                }

                Some(specialized.clone())
            }
            (Pattern::Variable(_), Pattern::Variable(var)) => {
                Some([Pattern::Variable(var.clone())].to_vec())
            }

            (Pattern::Enum(path1, patterns1), Pattern::Enum(path2, patterns2)) => {
                if path1 != path2 {
                    return None;
                }
                match (patterns1, patterns2) {
                    (Some(p1), Some(p2)) => {
                        if p1.len() != p2.len() {
                            //Not sure this is necessary
                            return None;
                        }
                        let pats = patterns2.as_ref().unwrap();
                        Some(pats.clone())
                    }

                    (None, _) => None,

                    (Some(_), None) => None,
                }
            }
            _ => None,
        }
    }

    // Use a pattern to unspecialize a list of patterns. This is the inverse of specialize.
    pub fn unspecialize(&self, data: &[Pattern]) -> Option<Vec<Pattern>> {
        match (data, self) {
            ([], _) => Some(vec![self.clone()]),
            (data, Pattern::CatchAll) => Some(vec![data[0].clone()]),
            (data, Pattern::Variable(_)) => {
                if let Pattern::Variable(v) = &data[0] {
                    Some(vec![Pattern::Variable(v.clone())])
                } else {
                    None
                }
            }
            (data, Pattern::Number(_)) => {
                if let Pattern::Number(n) = &data[0] {
                    Some(vec![Pattern::Number(n.clone())])
                } else {
                    None
                }
            }
            (data, Pattern::String(_)) => {
                if let Pattern::String(s) = &data[0] {
                    Some(vec![Pattern::String(s.clone())])
                } else {
                    None
                }
            }
            (data, Pattern::Tuple(cons_patterns)) => {
                let length = cons_patterns.len();
                if data.len() >= length {
                    Some(vec![Pattern::Tuple(
                        data.iter().take(length).cloned().collect::<Vec<Pattern>>(),
                    )])
                } else {
                    None
                }
            }
            (data, Pattern::Array(cons_patterns)) => {
                let length = cons_patterns.len();
                if data.len() >= length {
                    Some(vec![Pattern::Array(
                        data.iter().take(length).cloned().collect::<Vec<Pattern>>(),
                    )])
                } else {
                    None
                }
            }
            (data, Pattern::Enum(path1, patterns1)) => match patterns1 {
                Some(patterns1) => {
                    let length = patterns1.len();
                    if data.len() >= length {
                        Some(vec![Pattern::Enum(
                            path1.clone(),
                            Some(data.iter().take(length).cloned().collect::<Vec<Pattern>>()),
                        )])
                    } else {
                        None
                    }
                }
                None => Some(vec![Pattern::Enum(path1.clone(), None)]),
            },
            _ => unreachable!("Unspecialize with invalid pattern"),
        }
    }
}

impl Children<Pattern> for Pattern {
    fn children(&self) -> Box<dyn Iterator<Item = &Pattern> + '_> {
        match self {
            Pattern::CatchAll
            | Pattern::Ellipsis
            | Pattern::Number(_)
            | Pattern::String(_)
            | Pattern::Variable(_) => Box::new(empty()),
            Pattern::Tuple(p) | Pattern::Array(p) => Box::new(p.iter()),
            Pattern::Enum(_, fields) => Box::new(fields.iter().flatten()),
        }
    }

    fn children_mut(&mut self) -> Box<dyn Iterator<Item = &mut Pattern> + '_> {
        match self {
            Pattern::CatchAll
            | Pattern::Ellipsis
            | Pattern::Number(_)
            | Pattern::String(_)
            | Pattern::Variable(_) => Box::new(empty()),
            Pattern::Tuple(p) | Pattern::Array(p) => Box::new(p.iter_mut()),
            Pattern::Enum(_, fields) => Box::new(fields.iter_mut().flatten()),
        }
    }
}

#[cfg(test)]
mod test {
    use std::iter::zip;

    use super::*;

    #[test]
    fn test_specialize_catchall() {
        let cons = Pattern::Tuple(vec![
            Pattern::Number(1.into()),
            Pattern::Variable("x".to_string()),
        ]);

        let pat = Pattern::Tuple(vec![
            Pattern::Number(1.into()),
            Pattern::Variable("y".to_string()),
        ]);

        let specialized = pat.specialize(&cons);
        assert_eq!(
            specialized,
            Some(
                [
                    Pattern::Number(1.into()),
                    Pattern::Variable("y".to_string())
                ]
                .to_vec()
            )
        );

        let mut inners = Vec::new();
        for (con, spe) in zip(cons.children(), specialized.unwrap().iter()) {
            let inner = spe.specialize(con);
            if inner.is_some() {
                let value = inner.unwrap();
                inners.extend(value);
            }
        }

        assert_eq!(inners, vec![Pattern::Variable("y".to_string())]);
    }

    #[test]
    fn test_specialize_ellipsis() {
        let cons = Pattern::Array(vec![
            Pattern::Number(0.into()),
            Pattern::Number(0.into()),
            Pattern::Number(0.into()),
            Pattern::Number(0.into()),
        ]);

        let pat = Pattern::Array(vec![
            Pattern::Number(1.into()),
            Pattern::Ellipsis,
            Pattern::Number(2.into()),
        ]);

        let specialized = pat.specialize(&cons);

        assert_eq!(
            specialized,
            Some(vec![
                Pattern::Number(1.into()),
                Pattern::CatchAll,
                Pattern::CatchAll,
                Pattern::Number(2.into()),
            ])
        );
    }

    #[test]
    fn test_specialize_enum() {
        let cons = Pattern::Enum(
            SymbolPath::from_str("Foo").unwrap(),
            Some(vec![
                Pattern::Number(0.into()),
                Pattern::Variable("x".to_string()),
                Pattern::CatchAll,
            ]),
        );

        let pat = Pattern::Enum(
            SymbolPath::from_str("Foo").unwrap(),
            Some(vec![
                Pattern::Variable("x".to_string()),
                Pattern::Variable("y".to_string()),
                Pattern::Variable("z".to_string()),
            ]),
        );

        let specialized = pat.specialize(&cons);
        assert_eq!(
            specialized,
            Some(
                [
                    Pattern::Variable("x".to_string()),
                    Pattern::Variable("y".to_string()),
                    Pattern::Variable("z".to_string())
                ]
                .to_vec()
            )
        );
    }

    #[test]
    fn test_specialize_enum_different_symbolpath() {
        let cons = Pattern::Enum(
            SymbolPath::from_str("Foo1").unwrap(),
            Some(vec![
                Pattern::Number(0.into()),
                Pattern::Variable("x".to_string()),
                Pattern::CatchAll,
            ]),
        );

        let pat = Pattern::Enum(
            SymbolPath::from_str("Foo").unwrap(),
            Some(vec![
                Pattern::Variable("x".to_string()),
                Pattern::Variable("y".to_string()),
                Pattern::Variable("z".to_string()),
            ]),
        );

        let specialized = pat.specialize(&cons);
        assert_eq!(specialized, None);
    }
}
