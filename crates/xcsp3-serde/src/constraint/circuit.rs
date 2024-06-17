use serde::{Deserialize, Serialize};

use crate::{
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
pub struct Circuit<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(
		alias = "$text",
		deserialize_with = "deserialize_int_exps",
		serialize_with = "serialize_list"
	)]
	pub list: Vec<IntExp<Identifier>>, // TODO: "startIndex" attribute
	#[serde(default = "zero", skip_serializing_if = "is_zero")]
	pub size: IntExp<Identifier>,
}

fn zero<I>() -> IntExp<I> {
	IntExp::Const(0)
}
fn is_zero<I>(val: &IntExp<I>) -> bool {
	matches!(val, IntExp::Const(0))
}
