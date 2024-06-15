use std::{fmt::Display, str::FromStr};

use serde::{Deserialize, Serialize};

use crate::{
	constraint::{deserialize_transitions, ConstraintMeta, Transition},
	parser::{
		integer::{deserialize_int_exps, IntExp},
		serialize_list,
	},
};

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Mdd<Identifier = String> {
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
}
