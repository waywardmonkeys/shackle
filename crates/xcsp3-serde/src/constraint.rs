pub(crate) mod extension;
pub(crate) mod intension;

use serde::{Deserialize, Serialize};

use crate::constraint::{extension::Extension, intension::Intension};

#[derive(Debug, PartialEq, Clone, Deserialize, Serialize)]
#[serde(bound(
	deserialize = "Identifier: std::str::FromStr",
	serialize = "Identifier: std::fmt::Display"
))]
pub enum Constraint<Identifier = String> {
	#[serde(rename = "extension")]
	Extension(Extension<Identifier>),
	#[serde(rename = "intension")]
	Intension(Intension<Identifier>),
}
