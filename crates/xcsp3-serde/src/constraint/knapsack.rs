use serde::{Deserialize, Serialize};

use crate::{
	constraint::{Condition, ConstraintMeta},
	parser::integer::{
		deserialize_int_exps, deserialize_int_vals, serialize_int_exps, serialize_int_vals, IntExp,
	},
	IntVal,
};

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(
	deserialize = "Identifier: std::str::FromStr",
	serialize = "Identifier: std::fmt::Display"
))]
pub struct Knapsack<Identifier = String> {
	#[serde(flatten)]
	pub info: ConstraintMeta<Identifier>,
	#[serde(
		deserialize_with = "deserialize_int_exps",
		serialize_with = "serialize_int_exps"
	)]
	pub list: Vec<IntExp<Identifier>>,
	#[serde(
		deserialize_with = "deserialize_int_vals",
		serialize_with = "serialize_int_vals"
	)]
	pub weights: Vec<IntVal>,
	#[serde(
		deserialize_with = "deserialize_int_vals",
		serialize_with = "serialize_int_vals"
	)]
	pub profits: Vec<IntVal>,
	/// The first `Condition` element is related to weights whereas the second [`Condition`] element is related to profits.
	pub condition: [Condition<Identifier>; 2],
}
