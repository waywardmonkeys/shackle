use std::{fmt::Display, str::FromStr};

use serde::{Deserialize, Serialize};

use crate::{
	constraint::Condition,
	parser::{
		integer::{deserialize_int_exps, deserialize_int_vals, IntExp},
		serialize_list,
	},
	IntVal, MetaInfo,
};

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Sum<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(
		deserialize_with = "deserialize_int_exps",
		serialize_with = "serialize_list"
	)]
	pub list: Vec<IntExp<Identifier>>,
	#[serde(
		default,
		skip_serializing_if = "Vec::is_empty",
		deserialize_with = "deserialize_int_vals",
		serialize_with = "serialize_list"
	)]
	pub coeffs: Vec<IntVal>,
	pub condition: Condition<Identifier>,
}
