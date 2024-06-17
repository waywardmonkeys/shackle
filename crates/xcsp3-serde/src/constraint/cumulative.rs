use serde::{Deserialize, Serialize};

use crate::{
	constraint::Condition,
	parser::{
		integer::{deserialize_int_exps, IntExp},
		serialize_list,
	},
	MetaInfo,
};

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(
	deserialize = "Identifier: std::str::FromStr",
	serialize = "Identifier: std::fmt::Display"
))]
pub struct Cumulative<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(
		deserialize_with = "deserialize_int_exps",
		serialize_with = "serialize_list"
	)]
	pub origins: Vec<IntExp<Identifier>>,
	#[serde(
		deserialize_with = "deserialize_int_exps",
		serialize_with = "serialize_list"
	)]
	pub lengths: Vec<IntExp<Identifier>>,
	#[serde(
		deserialize_with = "deserialize_int_exps",
		serialize_with = "serialize_list"
	)]
	pub heights: Vec<IntExp<Identifier>>,
	pub condition: Condition<Identifier>,
}

// TODO: k-dimensional no-overlap constraint
