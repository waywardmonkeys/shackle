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
pub struct Element<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(
		alias = "$text",
		deserialize_with = "deserialize_int_exps",
		serialize_with = "serialize_list"
	)]
	pub list: Vec<IntExp<Identifier>>,
	#[serde(default, skip_serializing_if = "Option::is_none")]
	pub index: Option<IntExp<Identifier>>,
	#[serde(default, skip_serializing_if = "Option::is_none")]
	pub value: Option<IntExp<Identifier>>,
	#[serde(default, skip_serializing_if = "Option::is_none")]
	pub condition: Option<Condition<Identifier>>,
}
