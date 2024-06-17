use std::{fmt::Display, str::FromStr};

use serde::{Deserialize, Serialize};

use crate::{
	parser::{
		integer::{deserialize_int_exps, deserialize_int_vals, IntExp},
		serialize_list,
	},
	IntVal, MetaInfo,
};

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Objectives<Identifier = String> {
	#[serde(default, rename = "@combination")]
	pub combination: CombinationType,
	#[serde(rename = "$value")]
	pub objectives: Vec<Objective<Identifier>>,
}

impl<Identifier> Objectives<Identifier> {
	pub fn is_empty(&self) -> bool {
		self.objectives.is_empty()
	}
}

impl<Identifier> Default for Objectives<Identifier> {
	fn default() -> Self {
		Self {
			combination: CombinationType::default(),
			objectives: Vec::new(),
		}
	}
}

#[derive(Clone, Debug, Default, PartialEq, Hash, Deserialize, Serialize)]
pub enum CombinationType {
	#[serde(rename = "lexico")]
	#[default]
	Lexico,
	#[serde(rename = "pareto")]
	Pareto,
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub enum Objective<Identifier = String> {
	#[serde(rename = "minimize")]
	Minimize(ObjExp<Identifier>),
	#[serde(rename = "maximize")]
	Maximize(ObjExp<Identifier>),
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct ObjExp<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(alias = "@type", default)]
	pub ty: ObjType,
	#[serde(
		alias = "$text",
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
}

#[derive(Clone, Debug, Default, PartialEq, Hash, Deserialize, Serialize)]
pub enum ObjType {
	#[default]
	#[serde(rename = "sum")]
	Sum,
	#[serde(rename = "minimum")]
	Minimum,
	#[serde(rename = "maximum")]
	Maximum,
	#[serde(rename = "nValues")]
	NValues,
	#[serde(rename = "lex")]
	Lex,
}
