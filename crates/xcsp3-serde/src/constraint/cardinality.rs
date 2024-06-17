use std::{fmt::Display, str::FromStr};

use serde::{Deserialize, Serialize};

use crate::{
	parser::{
		deserialize_exps,
		integer::{deserialize_int_exps, IntExp},
		serialize_list, Exp,
	},
	MetaInfo,
};

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Cardinality<Identifier = String> {
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
	pub values: Vec<IntExp<Identifier>>, // TODO: "closed" attribute
	#[serde(
		deserialize_with = "deserialize_exps",
		serialize_with = "serialize_list"
	)]
	pub occurs: Vec<Exp<Identifier>>,
}
