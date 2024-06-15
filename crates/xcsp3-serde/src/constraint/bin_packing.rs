use serde::{Deserialize, Serialize};

use crate::{
	constraint::{Condition, ConstraintMeta},
	parser::{
		integer::{deserialize_int_exps, IntExp},
		serialize_list,
	},
};

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(
	deserialize = "Identifier: std::str::FromStr",
	serialize = "Identifier: std::fmt::Display"
))]
pub struct BinPacking<Identifier = String> {
	#[serde(flatten)]
	pub info: ConstraintMeta<Identifier>,
	#[serde(
		deserialize_with = "deserialize_int_exps",
		serialize_with = "serialize_list"
	)]
	pub list: Vec<IntExp<Identifier>>,
	#[serde(
		deserialize_with = "deserialize_int_exps",
		serialize_with = "serialize_list"
	)]
	pub sizes: Vec<IntExp<Identifier>>,
	#[serde(default, skip_serializing_if = "Option::is_none")]
	pub condition: Option<Condition<Identifier>>,
	#[serde(
		default,
		skip_serializing_if = "Vec::is_empty",
		deserialize_with = "deserialize_int_exps",
		serialize_with = "serialize_list"
	)]
	pub limits: Vec<IntExp<Identifier>>,
	#[serde(
		default,
		skip_serializing_if = "Vec::is_empty",
		deserialize_with = "deserialize_int_exps",
		serialize_with = "serialize_list"
	)]
	pub loads: Vec<IntExp<Identifier>>,
}

// TODO: Should `condition`, `limits` and `loads` be made mutually exclusive in the struct?
