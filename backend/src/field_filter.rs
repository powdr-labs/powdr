/// Creates a new `pub(crate)` general Factory struct from the RestrictedFactory
/// struct.
///
/// If the field type of the general factory is one of the listed supported types,
/// the macro will forward the calls to the restricted factory. Otherwise, it will
/// panic.
///
/// # Example
/// ```
/// generalize_factory!(Factory <- RestrictedFactory, [GoldilocksField, BabyBearField]);
/// ```
macro_rules! generalize_factory {
    ($general_factory:ident <- $restricted_factory:ident, [$($supported_type:ty),*]) => {
        pub(crate) struct $general_factory;

        impl<F: powdr_number::FieldElement> crate::BackendFactory<F> for $general_factory {
            fn create(
                &self,
                pil: std::sync::Arc<powdr_ast::analyzed::Analyzed<F>>,
                fixed: std::sync::Arc<std::vec::Vec<(
                    std::string::String,
                    powdr_executor::constant_evaluator::VariablySizedColumn<F>
                )>>,
                output_dir: std::option::Option<std::path::PathBuf>,
                setup: std::option::Option<&mut dyn std::io::Read>,
                verification_key: std::option::Option<&mut dyn std::io::Read>,
                verification_app_key: std::option::Option<&mut dyn std::io::Read>,
                backend_options: crate::BackendOptions,
            ) -> std::result::Result<Box<dyn crate::Backend<F>>, crate::Error> {
                use crate::Backend;
                use std::{any::TypeId, boxed::Box, result::Result::Ok};

                let result = match TypeId::of::<F>() {
                    $(
                        id if id == TypeId::of::<$supported_type>() => {
                            unsafe {
                                let result = <$restricted_factory as BackendFactory<$supported_type>>::create(
                                    &$restricted_factory,
                                    std::mem::transmute(pil), std::mem::transmute(fixed), output_dir, setup,
                                    verification_key, verification_app_key, backend_options)?;
                                let result: Box<dyn Backend<$supported_type>> = result;
                                std::mem::transmute(result)
                            }
                        }
                    )*
                    _ => panic!("Unsupported field type: {:?}", TypeId::of::<F>()),
                };
                Ok(result)
            }

            fn generate_setup(
                &self,
                size: powdr_number::DegreeType,
                output: &mut dyn std::io::Write
            ) -> std::result::Result<(), crate::Error> {
                use std::any::TypeId;
                match TypeId::of::<F>() {
                    $(
                        id if id == TypeId::of::<$supported_type>() => {
                            return <$restricted_factory as crate::BackendFactory<$supported_type>>::
                                generate_setup(&$restricted_factory, size, output)
                        }
                    )*
                    _ => panic!("Unsupported field type: {:?}", TypeId::of::<F>()),
                }
            }
        }
    };
}

pub(crate) use generalize_factory;
