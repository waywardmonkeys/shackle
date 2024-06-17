use std::{fmt::Display, str::FromStr};

use serde::{Deserialize, Serialize};

use crate::MetaInfo;
use crate::{
	constraint::Operator,
	parser::{
		integer::{deserialize_int_exps, IntExp},
		serialize_list,
	},
};

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Ordered<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(
		alias = "$text",
		deserialize_with = "deserialize_int_exps",
		serialize_with = "serialize_list"
	)]
	pub list: Vec<IntExp<Identifier>>,
	#[serde(
		default,
		skip_serializing_if = "Vec::is_empty",
		deserialize_with = "deserialize_int_exps",
		serialize_with = "serialize_list"
	)]
	pub lengths: Vec<IntExp<Identifier>>,
	pub operator: Operator,
}
