use std::{fmt::Display, str::FromStr};

use serde::{Deserialize, Serialize};

use super::ConstraintMeta;
use crate::{
	parser::integer::{
		deserialize_int_exps, deserialize_int_vals, serialize_int_exps, serialize_int_vals, IntExp,
	},
	IntVal,
};

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct AllDifferent<Identifier = String> {
	#[serde(flatten)]
	pub info: ConstraintMeta<Identifier>,
	#[serde(
		alias = "$text",
		deserialize_with = "deserialize_int_exps",
		serialize_with = "serialize_int_exps"
	)]
	pub list: Vec<IntExp<Identifier>>,
	#[serde(
		default,
		skip_serializing_if = "Vec::is_empty",
		deserialize_with = "deserialize_int_vals",
		serialize_with = "serialize_int_vals"
	)]
	pub except: Vec<IntVal>,
}
