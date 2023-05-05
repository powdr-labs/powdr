//! Formatting functions for analyzed PIL files.
//!
//! These are not meant to be 1-1 reproductions, they will have errors.
//! Do not use this to re-generate PIL files!

use std::fmt::{Display, Formatter, Result};

use itertools::Itertools;

use super::*;

impl<T: Display> Display for Analyzed<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for (name, value) in &self.constants {
            writeln!(f, "constant {name} = {value};")?;
        }

        let mut namespace = "Global".to_string();
        let mut update_namespace = |name: &str, degree: DegreeType, f: &mut Formatter<'_>| {
            if let Some(dot) = name.find('.') {
                if name[..dot] != namespace {
                    namespace = name[..dot].to_string();
                    writeln!(f, "namespace {namespace}({degree});")?;
                }
                Ok(name[dot + 1..].to_string())
            } else {
                Ok(name.to_string())
            }
        };

        for statement in &self.source_order {
            match statement {
                StatementIdentifier::Definition(name) => {
                    let (poly, definition) = &self.definitions[name];
                    let name = update_namespace(name, poly.degree, f)?;
                    let kind = match &poly.poly_type {
                        PolynomialType::Committed => "witness ",
                        PolynomialType::Constant => "fixed ",
                        PolynomialType::Intermediate => "",
                    };
                    write!(f, "    col {kind}{name}")?;
                    if let Some(value) = definition {
                        writeln!(f, "{value};")?
                    } else {
                        writeln!(f, ";")?
                    }
                }
                StatementIdentifier::PublicDeclaration(name) => {
                    let decl = &self.public_declarations[name];
                    // TODO we do not know the degree of the namespace here.
                    let name = update_namespace(&decl.name, 0, f)?;
                    writeln!(
                        f,
                        "    public {name} = {}({});",
                        decl.polynomial, decl.index
                    )?;
                }
                StatementIdentifier::Identity(i) => writeln!(f, "    {}", &self.identities[*i])?,
            }
        }

        Ok(())
    }
}

impl<T: Display> Display for FunctionValueDefinition<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FunctionValueDefinition::Mapping(e) => write!(f, "(i) {{ {e} }}"),
            FunctionValueDefinition::Array(items) => {
                write!(f, " = {}", items.iter().map(|i| i.to_string()).join(" + "))
            }
            FunctionValueDefinition::Query(e) => write!(f, "(i) query {e}"),
        }
    }
}

impl<T: Display> Display for RepeatedArray<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.repetitions == 0 {
            return Ok(());
        }
        write!(
            f,
            "[{}]",
            self.values.iter().map(|i| i.to_string()).join(", ")
        )?;
        if self.repetitions > 1 {
            write!(f, "*")?;
        }
        Ok(())
    }
}

impl<T: Display> Display for Identity<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.kind {
            IdentityKind::Polynomial => {
                let expression = self.left.selector.as_ref().unwrap();
                if let Expression::BinaryOperation(left, BinaryOperator::Sub, right) = expression {
                    write!(f, "{left} = {right};")
                } else {
                    write!(f, "{expression} = 0;")
                }
            }
            IdentityKind::Plookup => write!(f, "{} in {};", self.left, self.right),
            IdentityKind::Permutation => write!(f, "{} is {};", self.left, self.right),
            IdentityKind::Connect => write!(f, "{} connect {};", self.left, self.right),
        }
    }
}

impl<T: Display> Display for SelectedExpressions<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{{ {} }}",
            self.selector
                .as_ref()
                .map(|s| format!("{s} "))
                .unwrap_or_default(),
            format_expressions(&self.expressions)
        )
    }
}

impl<T: Display> Display for Expression<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Expression::Constant(name) => write!(f, "{name}"),
            Expression::PolynomialReference(reference) => write!(f, "{reference}"),
            Expression::PublicReference(name) => write!(f, ":{name}"),
            Expression::Number(value) => write!(f, "{value}"),
            Expression::String(value) => write!(f, "\"{value}\""), // TODO quote?
            Expression::Tuple(items) => write!(f, "({})", format_expressions(items)),
            Expression::BinaryOperation(left, op, right) => write!(f, "({left} {op} {right})"),
            Expression::UnaryOperation(op, exp) => write!(f, "{op}{exp}"),
            Expression::FunctionCall(fun, args) => write!(f, "{fun}({})", format_expressions(args)),
            Expression::LocalVariableReference(index) => {
                // TODO this is not really reproducing the input, but
                // if we want to do that, we would need the names of the local variables somehow.
                if *index == 0 {
                    write!(f, "i")
                } else {
                    write!(f, "${index}")
                }
            }
            Expression::MatchExpression(scrutinee, arms) => write!(
                f,
                "match {scrutinee} {{ {} }}",
                arms.iter()
                    .map(|(n, e)| format!(
                        "{} => {e},",
                        n.as_ref()
                            .map(|n| n.to_string())
                            .unwrap_or_else(|| "_".to_string())
                    ))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
        }
    }
}

fn format_expressions<T: Display>(expressions: &[Expression<T>]) -> String {
    expressions
        .iter()
        .map(|e| format!("{e}"))
        .collect::<Vec<_>>()
        .join(", ")
}

impl Display for PolynomialReference {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}{}{}",
            self.name,
            self.index
                .as_ref()
                .map(|s| format!("[{s}]"))
                .unwrap_or_default(),
            if self.next { "'" } else { "" }
        )
    }
}

#[cfg(test)]
mod test {
    use number::GoldilocksField;

    use crate::pil_analyzer::process_pil_file_contents;

    #[test]
    fn parse_print_analyzed() {
        let input = r#"constant %N = 65536;
    public P = T.pc(2);
namespace Bin(65536);
    col witness bla;
namespace T(65536);
    col fixed first_step = [1] + [0]*;
    col fixed line(i) { i };
    col witness pc;
    col witness XInv;
    col witness XIsZero;
    T.XIsZero = (1 - (T.X * T.XInv));
    (T.XIsZero * T.X) = 0;
    (T.XIsZero * (1 - T.XIsZero)) = 0;
    col witness instr_jmpz;
    col witness instr_jmpz_param_l;
    col witness instr_jmp;
    col witness instr_jmp_param_l;
    col witness instr_dec_CNT;
    col witness instr_assert_zero;
    (T.instr_assert_zero * (T.XIsZero - 1)) = 0;
    col witness X;
    col witness X_const;
    col witness X_read_free;
    col witness A;
    col witness CNT;
    col witness read_X_A;
    col witness read_X_CNT;
    col witness reg_write_X_CNT;
    col witness read_X_pc;
    col witness reg_write_X_A;
    T.X = ((((T.read_X_A * T.A) + (T.read_X_CNT * T.CNT)) + T.X_const) + (T.X_read_free * T.X_free_value));
    T.A' = (((T.first_step' * 0) + (T.reg_write_X_A * T.X)) + ((1 - (T.first_step' + T.reg_write_X_A)) * T.A));
    col witness X_free_value(i) query match T.pc { 0 => ("input", 1), 3 => ("input", (T.CNT + 1)), 7 => ("input", 0), };
    col fixed p_X_const = [0, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
    col fixed p_X_read_free = [1, 0, 0, 1, 0, 0, 0, -1, 0] + [0]*;
    col fixed p_read_X_A = [0, 0, 0, 1, 0, 0, 0, 1, 1] + [0]*;
    col fixed p_read_X_CNT = [0, 0, 1, 0, 0, 0, 0, 0, 0] + [0]*;
    col fixed p_read_X_pc = [0, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
    col fixed p_reg_write_X_A = [0, 0, 0, 1, 0, 0, 0, 1, 0] + [0]*;
    col fixed p_reg_write_X_CNT = [1, 0, 0, 0, 0, 0, 0, 0, 0] + [0]*;
    { T.pc, T.reg_write_X_A, T.reg_write_X_CNT } in (1 - T.first_step) { T.line, T.p_reg_write_X_A, T.p_reg_write_X_CNT };
"#;
        let formatted = process_pil_file_contents::<GoldilocksField>(input).to_string();
        if input != formatted {
            for (i, f) in input.split('\n').zip(formatted.split('\n')) {
                assert_eq!(i, f);
            }
        }
        assert_eq!(input, formatted);
    }
}
