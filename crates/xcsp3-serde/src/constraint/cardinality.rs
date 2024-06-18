use std::{fmt::Display, str::FromStr};

use serde::{Deserialize, Deserializer, Serialize};

use crate::{
	parser::{
		deserialize_exps,
		integer::{deserialize_int_exps, IntExp},
		serialize_list, Exp,
	},
	MetaInfo,
};

#[derive(Clone, Debug, PartialEq, Hash)]

pub struct Cardinality<Identifier = String> {
	pub info: MetaInfo<Identifier>,
	pub list: Vec<IntExp<Identifier>>,
	pub values: Vec<IntExp<Identifier>>,
	pub closed: bool,
	pub occurs: Vec<Exp<Identifier>>,
}

impl<'de, Identifier: FromStr> Deserialize<'de> for Cardinality<Identifier> {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		#[derive(Deserialize)]
		#[serde(bound(deserialize = "Identifier: FromStr"))]
		struct Values<Identifier> {
			#[serde(default, rename = "@closed")]
			closed: Option<bool>,
			#[serde(rename = "$text", deserialize_with = "deserialize_int_exps")]
			list: Vec<IntExp<Identifier>>,
		}
		#[derive(Deserialize)]
		#[serde(bound(deserialize = "Identifier: FromStr"))]
		struct Cardinality<Identifier = String> {
			#[serde(flatten)]
			info: MetaInfo<Identifier>,
			#[serde(deserialize_with = "deserialize_int_exps")]
			list: Vec<IntExp<Identifier>>,
			values: Values<Identifier>,
			#[serde(deserialize_with = "deserialize_exps")]
			occurs: Vec<Exp<Identifier>>,
		}
		let x = Cardinality::deserialize(deserializer)?;
		Ok(Self {
			info: x.info,
			list: x.list,
			values: x.values.list,
			closed: x.values.closed.unwrap_or(false),
			occurs: x.occurs,
		})
	}
}

impl<Identifier: Display> Serialize for Cardinality<Identifier> {
	fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		#[derive(Serialize)]
		#[serde(bound(serialize = "Identifier: Display"))]
		struct Values<'a, Identifier> {
			#[serde(rename = "@closed", skip_serializing_if = "is_false")]
			closed: bool,
			#[serde(rename = "$text", serialize_with = "serialize_list")]
			list: &'a Vec<IntExp<Identifier>>,
		}
		#[derive(Serialize)]
		#[serde(bound(serialize = "Identifier: Display"))]
		struct Cardinality<'a, Identifier = String> {
			#[serde(flatten)]
			info: &'a MetaInfo<Identifier>,
			#[serde(serialize_with = "serialize_list")]
			list: &'a Vec<IntExp<Identifier>>,
			values: Values<'a, Identifier>,
			#[serde(serialize_with = "serialize_list")]
			occurs: &'a Vec<Exp<Identifier>>,
		}
		let x = Cardinality {
			info: &self.info,
			list: &self.list,
			values: Values {
				closed: self.closed,
				list: &self.values,
			},
			occurs: &self.occurs,
		};
		x.serialize(serializer)
	}
}

fn is_false(x: &bool) -> bool {
	!x
}
