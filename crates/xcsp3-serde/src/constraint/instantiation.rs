use serde::{Deserialize, Serialize};

use crate::{
	parser::{integer::deserialize_int_vals, serialize_list},
	variable::{deserialize_var_refs, serialize_var_refs, VarRef},
	IntVal, MetaInfo,
};

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(
	deserialize = "Identifier: std::str::FromStr",
	serialize = "Identifier: std::fmt::Display"
))]
pub struct Instantiation<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(
		deserialize_with = "deserialize_var_refs",
		serialize_with = "serialize_var_refs"
	)]
	pub list: Vec<VarRef<Identifier>>,
	#[serde(
		default,
		skip_serializing_if = "Vec::is_empty",
		deserialize_with = "deserialize_int_vals",
		serialize_with = "serialize_list"
	)]
	pub except: Vec<IntVal>,
}
