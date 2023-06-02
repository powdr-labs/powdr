use strum::{Display, EnumString, EnumVariantNames};

#[derive(Clone, EnumString, EnumVariantNames, Display)]
pub enum Backend {
    #[strum(serialize = "halo2")]
    Halo2,
}
