use serde::{Deserialize, Serialize};

use crate::constraint::extension::Extension;

pub(crate) mod extension;

#[derive(Debug, PartialEq, Clone, Deserialize, Serialize)]
#[serde(bound(
	deserialize = "Identifier: std::str::FromStr",
	serialize = "Identifier: std::fmt::Display"
))]
pub enum Constraint<Identifier = String> {
	#[serde(rename = "extension")]
	Extension(Extension<Identifier>),
}
