use powdr_number::BigInt;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use std::iter::{empty, once};

use super::{asm::SymbolPath, visitor::Children};

/// When a pattern is specialised, in some cases the algorithm will return
/// a list of its internal patterns. PatternTuple works as a wrapper that makes these
/// patterns identifiable and implements the specialisation in tuples following
/// rustc[1] implementation.
///
/// [1] https://doc.rust-lang.org/nightly/nightly-rustc/rustc_pattern_analysis/usefulness/index.html#specialization
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PatternTuple {
    pub patterns: Vec<Pattern>,
}

impl PatternTuple {
    pub fn is_empty(&self) -> bool {
        self.patterns.is_empty()
    }

    /// Check for irrefutable patterns in pattern tuples. Since pattern tuples can contain
    /// internal patterns, irrefutability only applies in cases of unique patterns (len == 1).

    /// Example:
    /// If (_, _) was obtained from specialization, it is not true that the pattern is irrefutable
    ///  because having more than one element implies that the evaluation is performed on internal patterns.
    /// If on the other hand the pattern is (Pattern::Tuple(_,_), ), irrefutability applies in the standard way.
    pub fn is_irrefutable(&self) -> bool {
        if self.patterns.len() == 1 {
            self.patterns[0].is_irrefutable()
        } else {
            false
        }
    }

    /// Specialize a pattern tuple based on a "constructor" pattern tuple passed as a parameter.
    /// Based on https://doc.rust-lang.org/nightly/nightly-rustc/rustc_pattern_analysis/usefulness/index.html#specialization.
    /// If the constructor is shared with the pattern, specialize the pattern removing
    /// "one layer" (the constructor) and returning internal patterns.
    /// Return None if the pattern can't be specialized with the constructor or if the pattern is empty.
    pub fn specialize(&self, constructor: &PatternTuple) -> Option<PatternTuple> {
        if self.patterns.is_empty() || self.patterns.len() != constructor.patterns.len() {
            return None;
        }

        let first = self.patterns.first().unwrap();
        let specialized = first.specialize(&constructor.patterns[0]);
        specialized.map(|PatternTuple { mut patterns }| {
            let rest = &self.patterns[1..];
            patterns.extend(rest.iter().cloned());
            PatternTuple { patterns }
        })
    }

    /// Unspecialize a pattern tuple based on a "constructor" pattern tuple passed as a parameter.
    /// Based on https://doc.rust-lang.org/nightly/nightly-rustc/rustc_pattern_analysis/usefulness/index.html##undoing-specialization.
    /// Construct a pattern based in the constructor using the values in the tuple as parameters
    /// If the constructor takes fewer parameters than there are in the tuple, the tuple is extended with the remaining parameters.
    /// Return None if the pattern can't be specialized with the constructor or if the pattern is empty.
    pub fn unspecialize(&self, mut tuple: PatternTuple) -> Option<PatternTuple> {
        if self.patterns.is_empty() {
            return None;
        }

        let constructor = self.patterns.first().unwrap();
        if tuple.patterns.is_empty() {
            Some(PatternTuple {
                patterns: vec![constructor.clone()],
            })
        } else {
            match constructor {
                Pattern::CatchAll => {
                    tuple.patterns.insert(0, Pattern::CatchAll);
                    Some(PatternTuple {
                        patterns: tuple.patterns,
                    })
                }
                Pattern::Number(n) => {
                    tuple.patterns.insert(0, Pattern::Number(n.clone()));
                    Some(PatternTuple {
                        patterns: tuple.patterns,
                    })
                }
                Pattern::String(s) => {
                    tuple.patterns.insert(0, Pattern::String(s.clone()));
                    Some(PatternTuple {
                        patterns: tuple.patterns,
                    })
                }
                Pattern::Variable(v) => {
                    tuple.patterns.insert(0, Pattern::Variable(v.clone()));
                    Some(PatternTuple {
                        patterns: tuple.patterns,
                    })
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

                Pattern::Enum(path1, _) => Some(PatternTuple {
                    patterns: vec![Pattern::Enum(path1.clone(), Some(tuple.patterns))],
                }),
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
        // TODO This should also return true for enum variants that are the only ones in the enum
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

    /// Specialize a pattern based on a "constructor" pattern passed as a parameter.
    /// Based on https://doc.rust-lang.org/nightly/nightly-rustc/rustc_pattern_analysis/usefulness/index.html#specialization.
    /// If the constructor is shared with the pattern, specialize the pattern removing
    /// "one layer" (the constructor) and returning internal patterns.
    /// Return None if the pattern can't be specialized using the constructor.
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
                if path1 != path2 {
                    return None;
                }
                match (patterns1, patterns2) {
                    (Some(_), Some(_)) => {
                        let pats = patterns2.as_ref().unwrap();
                        Some(PatternTuple {
                            patterns: pats.clone(),
                        })
                    }

                    _ => None,
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
    fn test_specialize_double() {
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
            Some(PatternTuple {
                patterns: [
                    Pattern::Number(1.into()),
                    Pattern::Variable("y".to_string())
                ]
                .to_vec()
            })
        );

        let mut inners = Vec::new();
        for (con, spe) in zip(cons.children(), specialized.unwrap().patterns) {
            let inner = spe.specialize(con);
            if let Some(value) = inner {
                if !value.is_empty() {
                    inners.push(value);
                }
            }
        }

        assert_eq!(inners, vec![Pattern::Variable("y".to_string()).into()]);
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
            Some(PatternTuple {
                patterns: [
                    Pattern::Number(1.into()),
                    Pattern::CatchAll,
                    Pattern::CatchAll,
                    Pattern::Number(2.into()),
                ]
                .to_vec()
            })
        );
    }

    #[test]
    fn test_specialize_enum() {
        let cons = Pattern::Enum(
            SymbolPath::from_identifier("Foo".to_string()),
            Some(vec![Pattern::Number(1.into())]),
        );

        let pat = Pattern::Enum(
            SymbolPath::from_identifier("Foo".to_string()),
            Some(vec![Pattern::Number(2.into())]),
        );

        let specialized = pat.specialize(&cons);

        assert_eq!(
            specialized,
            Some(PatternTuple {
                patterns: [Pattern::Number(2.into())].to_vec()
            })
        );
    }

    #[test]
    fn test_specialize_different_enums() {
        let cons = Pattern::Enum(
            SymbolPath::from_identifier("Foo".to_string()),
            Some(vec![Pattern::Number(1.into())]),
        );

        let pat = Pattern::Enum(
            SymbolPath::from_identifier("Foo2".to_string()),
            Some(vec![Pattern::Number(2.into())]),
        );

        let specialized = pat.specialize(&cons);

        assert_eq!(specialized, None);
    }

    #[test]
    fn test_specialize_different_arity_variants() {
        let cons = Pattern::Enum(
            SymbolPath::from_identifier("Foo".to_string()),
            Some(vec![Pattern::Number(1.into())]),
        );

        let pat = Pattern::Enum(
            SymbolPath::from_identifier("Foo".to_string()),
            Some(vec![Pattern::Number(2.into()), Pattern::Number(3.into())]),
        );

        let specialized = pat.specialize(&cons);

        assert_eq!(
            specialized,
            Some(PatternTuple {
                patterns: [Pattern::Number(2.into()), Pattern::Number(3.into())].to_vec()
            })
        );
    }
}
