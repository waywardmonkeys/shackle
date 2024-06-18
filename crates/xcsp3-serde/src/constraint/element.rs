use std::{fmt::Display, str::FromStr};

use serde::{Deserialize, Serialize};

use crate::{
	constraint::{Condition, OffsetList},
	parser::integer::IntExp,
	MetaInfo,
};

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Element<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	pub list: OffsetList<Identifier>,
	#[serde(default, skip_serializing_if = "Option::is_none")]
	pub index: Option<IntExp<Identifier>>,
	#[serde(default, skip_serializing_if = "Option::is_none")]
	pub value: Option<IntExp<Identifier>>,
	#[serde(default, skip_serializing_if = "Option::is_none")]
	pub condition: Option<Condition<Identifier>>,
}
