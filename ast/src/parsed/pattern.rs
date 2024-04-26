use powdr_number::BigInt;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use std::iter::{empty, once};

use super::{asm::SymbolPath, visitor::Children};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PatternTuple {
    pub patterns: Vec<Pattern>,
}

impl PatternTuple {
    pub fn is_empty(&self) -> bool {
        self.patterns.is_empty()
    }

    pub fn is_irrefutable(&self) -> bool {
        if self.patterns.len() != 1 {
            return false;
        }
        self.patterns[0].is_irrefutable()
    }

    pub fn specialize(&self, constructor: &PatternTuple) -> Option<PatternTuple> {
        if self.patterns.is_empty() || self.patterns.len() != constructor.patterns.len() {
            return None;
        }

        let first = self.patterns.first().unwrap();

        let mut patterns = self.patterns.clone();
        let rest = patterns.drain(1..).collect::<Vec<Pattern>>();
        let specialized = first.specialize(&constructor.patterns[0]);
        match specialized {
            Some(PatternTuple { mut patterns }) => {
                patterns.extend(rest);
                Some(PatternTuple { patterns })
            }
            None => None,
        }
    }

    pub fn unspecialize(&self, tuple: PatternTuple) -> Option<PatternTuple> {
        if self.patterns.is_empty() {
            return None;
        }

        let constructor = self.patterns.first().unwrap();
        if tuple.patterns.is_empty() {
            return Some(PatternTuple {
                patterns: vec![constructor.clone()],
            });
        } else {
            match constructor {
                Pattern::CatchAll => {
                    let mut patterns = tuple.patterns.clone();
                    patterns.insert(0, Pattern::CatchAll);
                    Some(PatternTuple { patterns })
                }
                Pattern::Number(n) => {
                    let mut patterns = tuple.patterns.clone();
                    patterns.insert(0, Pattern::Number(n.clone()));
                    Some(PatternTuple { patterns })
                }
                Pattern::String(s) => {
                    let mut patterns = tuple.patterns.clone();
                    patterns.insert(0, Pattern::String(s.clone()));
                    Some(PatternTuple { patterns })
                }
                Pattern::Variable(v) => {
                    let mut patterns = tuple.patterns.clone();
                    patterns.insert(0, Pattern::Variable(v.clone()));
                    Some(PatternTuple { patterns })
                }
                Pattern::Tuple(cons_patterns) => {
                    let length = cons_patterns.len();
                    if tuple.patterns.len() >= length {
                        let (to_unspecialize, remaining_part) = tuple.patterns.split_at(length);
                        let unspecialized = Pattern::Tuple(to_unspecialize.to_vec());

                        let mut new_patterns = Vec::with_capacity(remaining_part.len() + 1);
                        new_patterns.push(unspecialized);
                        new_patterns.extend_from_slice(remaining_part);
                        Some(PatternTuple {
                            patterns: new_patterns,
                        })
                    } else {
                        None
                    }
                }
                Pattern::Array(cons_patterns) => {
                    let length = cons_patterns.len();
                    if tuple.patterns.len() >= length {
                        let (to_unspecialize, remaining_part) = tuple.patterns.split_at(length);
                        let unspecialized = Pattern::Array(to_unspecialize.to_vec());

                        let mut new_patterns = Vec::with_capacity(remaining_part.len() + 1);
                        new_patterns.push(unspecialized);
                        new_patterns.extend_from_slice(remaining_part);
                        Some(PatternTuple {
                            patterns: new_patterns,
                        })
                    } else {
                        None
                    }
                }

                Pattern::Enum(path1, patterns1) => {
                    todo!("Enum")
                }
                _ => None,
            }
        }
    }
}

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize, JsonSchema, Hash,
)]
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
    pub fn specialize(&self, constructor: &Self) -> Option<PatternTuple> {
        match (constructor, self) {
            (_, Pattern::CatchAll) => Some(PatternTuple { patterns: vec![] }),
            (Pattern::CatchAll, _) => None,
            //(Pattern::CatchAll, Pattern::CatchAll) => Some(PatternTuple { patterns: vec![] }),
            (Pattern::Number(y), Pattern::Number(x)) => {
                (y == x).then_some(PatternTuple { patterns: vec![] })
            }
            (Pattern::String(r), Pattern::String(s)) => {
                (r == s).then_some(PatternTuple { patterns: vec![] })
            }

            (Pattern::Tuple(cons_patterns), Pattern::Tuple(patterns))
                if cons_patterns.len() == patterns.len() =>
            {
                Some(PatternTuple {
                    patterns: patterns.to_vec(),
                })
            }
            (Pattern::Array(cons_patterns), Pattern::Array(patterns)) => {
                let max_len = cons_patterns.len();
                let mut constructors = Vec::new();
                if patterns.contains(&Pattern::Ellipsis) {
                    let mut expanded_items = patterns.clone();
                    let ellipsis_index = patterns
                        .iter()
                        .position(|p| p == &Pattern::Ellipsis)
                        .unwrap();
                    let ellipsis_len = max_len - patterns.len() + 1;

                    expanded_items.remove(ellipsis_index);

                    expanded_items.splice(
                        ellipsis_index..ellipsis_index,
                        std::iter::repeat(Pattern::CatchAll).take(ellipsis_len),
                    );

                    constructors.extend(expanded_items);
                } else {
                    constructors = patterns.to_vec();
                }

                Some(PatternTuple {
                    patterns: constructors,
                })
            }
            (Pattern::Variable(_), Pattern::Variable(var)) => Some(PatternTuple {
                patterns: [Pattern::Variable(var.clone())].to_vec(),
            }),

            (Pattern::Enum(path1, patterns1), Pattern::Enum(path2, patterns2)) => {
                // TODO Rewrite
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
                        Some(PatternTuple {
                            patterns: pats.clone(),
                        })
                    }

                    (None, _) => None,

                    (Some(_), None) => None,
                }
            }
            _ => None,
        }
    }
}

impl From<Pattern> for PatternTuple {
    fn from(pattern: Pattern) -> Self {
        PatternTuple {
            patterns: vec![pattern],
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
        /*assert_eq!(
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
        */
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

        /*assert_eq!(
            specialized,
            Some(vec![
                Pattern::Number(1.into()),
                Pattern::CatchAll,
                Pattern::CatchAll,
                Pattern::Number(2.into()),
            ])
        );*/
    }

    #[test]
    fn test_specialize_enum() {
        let cons = Pattern::Enum(
            SymbolPath::from_identifier("Foo".to_string()),
            Some(vec![
                Pattern::Number(0.into()),
                Pattern::Variable("x".to_string()),
                Pattern::CatchAll,
            ]),
        );

        let pat = Pattern::Enum(
            SymbolPath::from_identifier("Foo".to_string()),
            Some(vec![
                Pattern::Variable("x".to_string()),
                Pattern::Variable("y".to_string()),
                Pattern::Variable("z".to_string()),
            ]),
        );

        let specialized = pat.specialize(&cons);
        /*assert_eq!(
            specialized,
            Some(
                [
                    Pattern::Variable("x".to_string()),
                    Pattern::Variable("y".to_string()),
                    Pattern::Variable("z".to_string())
                ]
                .to_vec()
            )
        );*/
    }

    #[test]
    fn test_specialize_enum_different_symbolpath() {
        let cons = Pattern::Enum(
            SymbolPath::from_identifier("Foo1".to_string()),
            Some(vec![
                Pattern::Number(0.into()),
                Pattern::Variable("x".to_string()),
                Pattern::CatchAll,
            ]),
        );

        let pat = Pattern::Enum(
            SymbolPath::from_identifier("Foo".to_string()),
            Some(vec![
                Pattern::Variable("x".to_string()),
                Pattern::Variable("y".to_string()),
                Pattern::Variable("z".to_string()),
            ]),
        );

        let specialized = pat.specialize(&cons);
        //assert_eq!(specialized, None);
    }
}
