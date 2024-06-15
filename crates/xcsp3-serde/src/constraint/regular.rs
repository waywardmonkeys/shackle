use std::{fmt::Display, str::FromStr};

use serde::{Deserialize, Serialize};

use crate::{
	constraint::{deserialize_transitions, ConstraintMeta, Transition},
	parser::{
		identifier::{deserialize_from_str, serialize_as_str},
		integer::{deserialize_int_exps, IntExp},
		serialize_list,
	},
};

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Regular<Identifier = String> {
	#[serde(flatten)]
	pub info: ConstraintMeta<Identifier>,
	#[serde(
		deserialize_with = "deserialize_int_exps",
		serialize_with = "serialize_list"
	)]
	pub list: Vec<IntExp<Identifier>>,
	#[serde(
		deserialize_with = "deserialize_transitions",
		serialize_with = "serialize_list"
	)]
	pub transitions: Vec<Transition<Identifier>>,
	#[serde(
		deserialize_with = "deserialize_from_str",
		serialize_with = "serialize_as_str"
	)]
	pub start: Identifier,
	#[serde(
		rename = "final",
		deserialize_with = "deserialize_from_str",
		serialize_with = "serialize_as_str"
	)]
	pub finish: Identifier,
}
