use serde::{Deserialize, Serialize};

use crate::{
	constraint::ConstraintMeta,
	parser::integer::{deserialize_int_exps, serialize_int_exps, IntExp},
};

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(
	deserialize = "Identifier: std::str::FromStr",
	serialize = "Identifier: std::fmt::Display"
))]
pub struct NoOverlap<Identifier = String> {
	#[serde(flatten)]
	pub info: ConstraintMeta<Identifier>,
	#[serde(
		deserialize_with = "deserialize_int_exps",
		serialize_with = "serialize_int_exps"
	)]
	pub origins: Vec<IntExp<Identifier>>,
	#[serde(
		deserialize_with = "deserialize_int_exps",
		serialize_with = "serialize_int_exps"
	)]
	pub lengths: Vec<IntExp<Identifier>>,
}

// TODO: k-dimensional no-overlap constraint
