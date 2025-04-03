#[cfg(any(feature = "estark-polygon", feature = "estark-starky"))]
mod estark;
#[cfg(feature = "halo2")]
mod halo2;
#[cfg(feature = "plonky3")]
mod plonky3;
#[cfg(feature = "stwo")]
mod stwo;

mod composite;
mod field_filter;
mod mock;

use powdr_ast::{
    object::Location,
    analyzed::{Analyzed, Reference, PolynomialReference, Expression},
    parsed::{types::Type, asm::SymbolPath, ArrayLiteral, Expression as ParsedExpression, FunctionCall, PILFile, PilStatement, NamespacedPolynomialReference, IndexAccess, BinaryOperation, UnaryOperation, Number},
};
use powdr_executor::{constant_evaluator::VariablySizedColumn, witgen::WitgenCallback};
use powdr_number::{DegreeType, FieldElement};
use powdr_parser_util::SourceRef;
use std::str::FromStr;
use std::{collections::BTreeMap, io, path::PathBuf, sync::Arc};
use strum::{Display, EnumString, EnumVariantNames};

#[derive(Clone, Default, EnumString, EnumVariantNames, Display, Copy)]
pub enum BackendType {
    #[strum(serialize = "mock")]
    #[default]
    Mock,
    #[cfg(feature = "halo2")]
    #[strum(serialize = "halo2")]
    Halo2,
    #[cfg(feature = "halo2")]
    #[strum(serialize = "halo2-composite")]
    Halo2Composite,
    #[cfg(feature = "halo2")]
    #[strum(serialize = "halo2-mock")]
    Halo2Mock,
    #[cfg(feature = "halo2")]
    #[strum(serialize = "halo2-mock-composite")]
    Halo2MockComposite,
    #[cfg(feature = "estark-polygon")]
    #[strum(serialize = "estark-polygon")]
    EStarkPolygon,
    #[cfg(feature = "estark-polygon")]
    #[strum(serialize = "estark-polygon-composite")]
    EStarkPolygonComposite,
    #[cfg(feature = "estark-starky")]
    #[strum(serialize = "estark-starky")]
    EStarkStarky,
    #[cfg(feature = "estark-starky")]
    #[strum(serialize = "estark-starky-composite")]
    EStarkStarkyComposite,
    #[cfg(feature = "estark-starky")]
    #[strum(serialize = "estark-dump")]
    EStarkDump,
    #[cfg(feature = "estark-starky")]
    #[strum(serialize = "estark-dump-composite")]
    EStarkDumpComposite,
    #[cfg(feature = "plonky3")]
    #[strum(serialize = "plonky3")]
    Plonky3,
    #[cfg(feature = "plonky3")]
    #[strum(serialize = "plonky3-composite")]
    Plonky3Composite,
    #[cfg(feature = "stwo")]
    #[strum(serialize = "stwo")]
    Stwo,
    #[cfg(feature = "stwo")]
    #[strum(serialize = "stwo-composite")]
    StwoComposite,
}

pub type BackendOptions = String;
pub const DEFAULT_HALO2_OPTIONS: &str = "poseidon";
pub const DEFAULT_HALO2_MOCK_OPTIONS: &str = "";
pub const DEFAULT_ESTARK_OPTIONS: &str = "stark_gl";

impl BackendType {
    pub fn factory<T: FieldElement>(&self) -> Box<dyn BackendFactory<T>> {
        match self {
            BackendType::Mock => Box::new(mock::MockBackendFactory),
            #[cfg(feature = "halo2")]
            BackendType::Halo2 => Box::new(halo2::Halo2ProverFactory),
            #[cfg(feature = "halo2")]
            BackendType::Halo2Composite => Box::new(composite::CompositeBackendFactory::new(
                halo2::Halo2ProverFactory,
            )),
            #[cfg(feature = "halo2")]
            BackendType::Halo2Mock => Box::new(halo2::Halo2MockFactory),
            #[cfg(feature = "halo2")]
            BackendType::Halo2MockComposite => Box::new(composite::CompositeBackendFactory::new(
                halo2::Halo2MockFactory,
            )),
            #[cfg(feature = "estark-polygon")]
            BackendType::EStarkPolygon => Box::new(estark::polygon_wrapper::Factory),
            #[cfg(feature = "estark-polygon")]
            BackendType::EStarkPolygonComposite => Box::new(
                composite::CompositeBackendFactory::new(estark::polygon_wrapper::Factory),
            ),
            #[cfg(feature = "estark-starky")]
            BackendType::EStarkStarky => Box::new(estark::starky_wrapper::Factory),
            #[cfg(feature = "estark-starky")]
            BackendType::EStarkStarkyComposite => Box::new(
                composite::CompositeBackendFactory::new(estark::starky_wrapper::Factory),
            ),
            // We need starky here because the dump backend uses some types that come from starky.
            #[cfg(feature = "estark-starky")]
            BackendType::EStarkDump => Box::new(estark::DumpFactory),
            #[cfg(feature = "estark-starky")]
            BackendType::EStarkDumpComposite => {
                Box::new(composite::CompositeBackendFactory::new(estark::DumpFactory))
            }
            #[cfg(feature = "plonky3")]
            BackendType::Plonky3 => Box::new(plonky3::Factory),
            #[cfg(feature = "plonky3")]
            BackendType::Plonky3Composite => {
                Box::new(composite::CompositeBackendFactory::new(plonky3::Factory))
            }
            #[cfg(feature = "stwo")]
            BackendType::Stwo => Box::new(stwo::Factory),
            #[cfg(feature = "stwo")]
            BackendType::StwoComposite => {
                Box::new(composite::CompositeBackendFactory::new(stwo::Factory))
            }
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("input/output error")]
    IO(#[from] std::io::Error),
    #[error("the witness is empty")]
    EmptyWitness,
    #[error("the backend has no setup operations")]
    NoSetupAvailable,
    #[error("the backend does not use a proving key setup")]
    NoProvingKeyAvailable,
    #[error("the backend does not implement proof verification")]
    NoVerificationAvailable,
    #[error("the backend does not support Ethereum onchain verification")]
    NoEthereumVerifierAvailable,
    #[error("the backend does not support proof aggregation")]
    NoAggregationAvailable,
    #[error("the backend does not support variable degrees")]
    NoVariableDegreeAvailable,
    #[error("internal backend error")]
    BackendError(String),
    #[error("the backend does not support public values which rely on later stage witnesses")]
    NoLaterStagePublicAvailable,
}

impl From<String> for Error {
    fn from(s: String) -> Self {
        Error::BackendError(s)
    }
}

pub type Proof = Vec<u8>;

/*
    Bellow are the public interface traits. They are implemented in this
    module, wrapping the traits implemented by each backend.
*/

/// Dynamic interface for a backend factory.
pub trait BackendFactory<F: FieldElement> {
    /// Create a new backend object.
    #[allow(clippy::too_many_arguments)]
    fn create(
        &self,
        pil: Arc<Analyzed<F>>,
        fixed: Arc<Vec<(String, VariablySizedColumn<F>)>>,
        output_dir: Option<PathBuf>,
        setup: Option<&mut dyn io::Read>,
        proving_key: Option<&mut dyn io::Read>,
        verification_key: Option<&mut dyn io::Read>,
        verification_app_key: Option<&mut dyn io::Read>,
        backend_options: BackendOptions,
    ) -> Result<Box<dyn Backend<F>>, Error>;

    /// Generate a new setup.
    fn generate_setup(&self, _size: DegreeType, _output: &mut dyn io::Write) -> Result<(), Error> {
        Err(Error::NoSetupAvailable)
    }

    fn specialize_pil(
        &self,
        pil: Analyzed<F>,
        bus_linker_args: Option<powdr_linker::BusLinkerArgs>,
        // common_definitions: BTreeMap<AbsoluteSymbolPath, Vec<PilStatement>>,
    ) -> Analyzed<F> {
        // Non bus mode current defaults to the identity function
        if bus_linker_args.is_none() {
            return pil;
        }

        println!("definitions 1:");
        pil.definitions.keys().for_each(|key| {
            println!("d1 key: {}", key);
        });


        // convert bus linker args to analyzed
        let proof_items: Vec<Expression> = bus_linker_args.unwrap().into_iter().map(
            |(location, args)| { // args is vector of tuples
                let tuple = args.into_iter().map(
                    |arg| { // a tuple
                        match arg {
                            ParsedExpression::Tuple(src, tuple) => {
                                // the last argument shows whether it's send or not
                                let is_send = match tuple.last().unwrap() {
                                    ParsedExpression::Reference(_, NamespacedPolynomialReference { path, .. }) => {
                                        match path.to_string().as_str() {
                                            "std::protocols::bus::BusLinkerType::Send" => true,
                                            "std::protocols::bus::BusLinkerType::PermutationReceive" => false,
                                            "std::protocols::bus::BusLinkerType::LookupReceive" => false,
                                            _ => panic!("Expected send or receive"),
                                        }
                                    }
                                    _ => panic!("Expected reference"),
                                };
                                let tuple_len = tuple.len();
                                let converted_tuple = tuple
                                    .into_iter()
                                    .enumerate()
                                    .map(|(index, item)| 
                                        if index == tuple_len - 1 {
                                            // do not add namespace to the bus type argument, which is in std
                                            convert_expr(item, &location, false, true)
                                        } else {
                                            // otherwise, add namespace to send arguments
                                            convert_expr(item, &location, is_send, true)
                                        }
                                
                                    )
                                    .collect::<Vec<_>>();

                                Expression::Tuple(src, converted_tuple)
                            },
                            _ => panic!("bus_linker_args must be a tuple"),
                        }
                    }
                ).collect();
                Expression::FunctionCall(
                    SourceRef::unknown(),
                    FunctionCall {
                        function: Box::new(Expression::Reference(
                            SourceRef::unknown(),
                            Reference::Poly(
                                PolynomialReference { name: "std::protocols::bus::bus_multi_linker".to_string(), type_args: None }
                            )
                        )),
                        arguments: vec![ArrayLiteral {
                            items: tuple,
                        }
                        .into()],
                    },
                )
            }
        ).collect();

        println!("definitions 2:");
        pil.definitions.keys().for_each(|key| {
            println!("d2 key: {}", key);
        });

        let analyzed = powdr_pil_analyzer::condenser::condense(
            pil.definitions,
            pil.solved_impls,
            &proof_items,
            pil.trait_impls,
            pil.source_order,
            pil.auto_added_symbols,
        );

        // // The following is bus mode
        // let pil_string = pil.to_string();
        // powdr_pilopt::maybe_write_pil(&pil_string, "specialize_pre_reparse").unwrap();

        // let parsed_pil = powdr_parser::parse(None, &pil_string).unwrap_or_else(|err| {
        //     eprintln!("Error parsing .pil file:");
        //     err.output_to_stderr();
        //     panic!();
        // });
        // powdr_pilopt::maybe_write_pil(&PILFile(parsed_pil.0.clone()), "specialize_post_reparse")
        //     .unwrap();

        // // powdr_pil_analyzer::analyze_ast::<F>(parsed_pil.clone()).unwrap();

        // // println!("bus_linker_args: \n{:#?}", bus_linker_args);

        // // TODO: make the following non-Stwo backends only.
        // let (mut pil_file_by_namespace, _) = parsed_pil.0.iter().fold(
        //     (BTreeMap::new(), String::new()),
        //     |(mut acc, mut namespace), pil_statement| {
        //         if let PilStatement::Namespace(_, symbol_path, _) = pil_statement {
        //             namespace = symbol_path.to_string();
        //         }
        //         acc.entry(namespace.clone())
        //             .or_insert_with(Vec::new)
        //             .push(pil_statement.clone());

        //         (acc, namespace)
        //     },
        // );

        // let pil_file_by_namespace_collapsed: Vec<PilStatement> = pil_file_by_namespace
        //     .iter()
        //     .flat_map(|(_, statements)| statements)
        //     .cloned()
        //     .collect();
        // powdr_pilopt::maybe_write_pil(
        //     &PILFile(pil_file_by_namespace_collapsed),
        //     "specialized_post_reparse_split",
        // )
        // .unwrap();

        // bus_linker_args.clone().unwrap().iter().for_each(
        //     // print
        //     |(loc, args)| {
        //         println!("bus_linker_args location: {}", loc);
        //         args.iter().for_each(|expr| {
        //             println!("bus_linker_args args: {}", expr);
        //         });
        //     },
        // );

        // bus_linker_args
        //     .unwrap()
        //     .iter()
        //     .for_each(|(namespace, bus_linker_args)| {
        //         pil_file_by_namespace
        //             .get_mut(&namespace.to_string())
        //             .expect("Namespace not found in pil_file_by_namespace")
        //             .push(PilStatement::Expression(
        //                 SourceRef::unknown(),
        //                 ParsedExpression::FunctionCall(
        //                     SourceRef::unknown(),
        //                     FunctionCall {
        //                         function: Box::new(ParsedExpression::Reference(
        //                             SourceRef::unknown(),
        //                             SymbolPath::from_str("std::protocols::bus::bus_multi_linker")
        //                                 .unwrap()
        //                                 .into(),
        //                         )),
        //                         arguments: vec![ArrayLiteral {
        //                             items: bus_linker_args.clone(),
        //                         }
        //                         .into()],
        //                     },
        //                 ),
        //             ));
        //     });

        // let all_statements = pil_file_by_namespace
        //     .into_iter()
        //     .flat_map(|(_, statements)| statements)
        //     .collect::<Vec<_>>();

        // powdr_pilopt::maybe_write_pil(&PILFile(all_statements.clone()), "specialized_pre_analyze")
        //     .unwrap();

        // log::debug!("SPECIALIZE: Analyzing PIL and computing constraints...");
        // let analyzed = powdr_pil_analyzer::analyze_string(&PILFile(all_statements).to_string()).unwrap();
        // log::debug!("SPECIALIZE: Analysis done.");

        powdr_pilopt::maybe_write_pil(&analyzed, "specialized_post_analyze").unwrap();

        analyzed
    }
}

fn convert_namespaced_reference(
    ns_ref: NamespacedPolynomialReference,
    location: &Location,
    add_namespace: bool,
) -> Reference {
    let name = if add_namespace {
        format!("{}::{}", location.to_string(), ns_ref.path)
    } else {
        ns_ref.path.to_string()
    };

    Reference::Poly(PolynomialReference {
        name,
        type_args: None,
    })
}

fn convert_number(
    num: Number,
    to_expr: bool
) -> Number {
    if to_expr {
        Number {
            value: num.value,
            type_: Some(Type::Expr),
        }
    } else {
        Number {
            value: num.value,
            type_: Some(Type::Int),
        }
    }
}

fn convert_expr(
    expr: ParsedExpression,
    location: &Location,
    add_namespace: bool,
    num_to_expr: bool,
) -> Expression {
    match expr {
        ParsedExpression::Reference(src, ns_ref) => {
            Expression::Reference(src, convert_namespaced_reference(ns_ref, location, add_namespace))
        },
        ParsedExpression::Number(src, num) => Expression::Number(src, convert_number(num, num_to_expr)),
        ParsedExpression::UnaryOperation(src, op) => {
            let converted_expr = Box::new(convert_expr(*op.expr, location, add_namespace, true));
            Expression::UnaryOperation(src, UnaryOperation { op: op.op, expr: converted_expr })
        },
        ParsedExpression::BinaryOperation(src, op) => {
            let converted_left = Box::new(convert_expr(*op.left, location, add_namespace, true));
            let converted_right = Box::new(convert_expr(*op.right, location, add_namespace, true));
            Expression::BinaryOperation(src, BinaryOperation {
                left: converted_left,
                op: op.op,
                right: converted_right,
            })
        },
        ParsedExpression::IndexAccess(src, idx_access) => {
            let converted_array = Box::new(convert_expr(*idx_access.array, location, add_namespace, true));
            let converted_index = Box::new(convert_expr(*idx_access.index, location, add_namespace, false)); // array index must be integer not expr
            Expression::IndexAccess(src, IndexAccess {
                array: converted_array,
                index: converted_index,
            })
        },
        ParsedExpression::ArrayLiteral(src, array) => {
            let converted_items = array
                .items
                .into_iter()
                .map(|item| convert_expr(item, location, add_namespace, true))
                .collect();
            Expression::ArrayLiteral(src, ArrayLiteral { items: converted_items })
        },
        _ => {
            panic!("Unexpected expression variant encountered during conversion: {expr}");
        }
    }
}

/// Dynamic interface for a backend.
pub trait Backend<F: FieldElement>: Send {
    /// Perform the proving.
    ///
    /// The backend uses the BackendOptions provided at creation time
    /// to potentially perform aggregation/compression.
    ///
    /// Returns the generated proof.
    fn prove(
        &self,
        witness: &[(String, Vec<F>)],
        publics: &BTreeMap<String, Option<F>>,
        prev_proof: Option<Proof>,
        witgen_callback: WitgenCallback<F>,
    ) -> Result<Proof, Error>;

    /// Verifies a proof.
    fn verify(&self, _proof: &[u8], _instances: &[Vec<F>]) -> Result<(), Error> {
        Err(Error::NoVerificationAvailable)
    }

    /// Exports the setup in a backend specific format. Can be used to create a
    /// new backend object of the same kind.
    fn export_setup(&self, _output: &mut dyn io::Write) -> Result<(), Error> {
        Err(Error::NoSetupAvailable)
    }

    /// Exports the verification key in a backend specific format. Can be used
    /// to create a new backend object of the same kind.
    fn export_verification_key(&self, output: &mut dyn io::Write) -> Result<(), Error> {
        let v = self.verification_key_bytes()?;
        log::info!("Verification key size: {} bytes", v.len());
        output
            .write_all(&v)
            .map_err(|_| Error::BackendError("Could not write verification key".to_string()))?;
        Ok(())
    }

    fn export_proving_key(&self, _output: &mut dyn io::Write) -> Result<(), Error> {
        Err(Error::NoProvingKeyAvailable)
    }

    fn verification_key_bytes(&self) -> Result<Vec<u8>, Error> {
        Err(Error::NoVerificationAvailable)
    }

    /// Exports an Ethereum verifier.
    fn export_ethereum_verifier(&self, _output: &mut dyn io::Write) -> Result<(), Error> {
        Err(Error::NoEthereumVerifierAvailable)
    }
}
