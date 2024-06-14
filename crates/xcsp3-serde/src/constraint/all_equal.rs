use serde::{Deserialize, Serialize};

use super::ConstraintMeta;
use crate::{
	parser::integer::{deserialize_int_vals, serialize_int_vals, IntExp},
	IntVal,
};

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(
	deserialize = "Identifier: std::str::FromStr",
	serialize = "Identifier: std::fmt::Display"
))]
pub struct AllEqual<Identifier = String> {
	#[serde(flatten)]
	info: ConstraintMeta<Identifier>,
	#[serde(
		alias = "$text",
		deserialize_with = "crate::parser::integer::deserialize_int_exps",
		serialize_with = "crate::parser::integer::serialize_int_exps"
	)]
	list: Vec<IntExp<Identifier>>,
	#[serde(
		default,
		skip_serializing_if = "Vec::is_empty",
		deserialize_with = "deserialize_int_vals",
		serialize_with = "serialize_int_vals"
	)]
	except: Vec<IntVal>,
}
