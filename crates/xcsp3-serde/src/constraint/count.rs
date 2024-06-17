use std::{fmt::Display, str::FromStr};

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
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Count<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(
		deserialize_with = "deserialize_int_exps",
		serialize_with = "serialize_list"
	)]
	pub list: Vec<IntExp<Identifier>>,
	#[serde(
		deserialize_with = "deserialize_int_exps",
		serialize_with = "serialize_list"
	)]
	pub values: Vec<IntExp<Identifier>>,
	pub condition: Condition<Identifier>,
}
