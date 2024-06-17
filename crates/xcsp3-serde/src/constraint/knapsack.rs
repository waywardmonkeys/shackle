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
#[serde(bound(
	deserialize = "Identifier: std::str::FromStr",
	serialize = "Identifier: std::fmt::Display"
))]
pub struct Knapsack<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(
		deserialize_with = "deserialize_int_exps",
		serialize_with = "serialize_list"
	)]
	pub list: Vec<IntExp<Identifier>>,
	#[serde(
		deserialize_with = "deserialize_int_vals",
		serialize_with = "serialize_list"
	)]
	pub weights: Vec<IntVal>,
	#[serde(
		deserialize_with = "deserialize_int_vals",
		serialize_with = "serialize_list"
	)]
	pub profits: Vec<IntVal>,
	/// The first `Condition` element is related to weights whereas the second [`Condition`] element is related to profits.
	pub condition: [Condition<Identifier>; 2],
}
