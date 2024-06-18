use std::{fmt::Display, str::FromStr};

use serde::{Deserialize, Deserializer, Serialize};

use crate::{
	parser::{
		integer::{deserialize_int_exps, deserialize_int_vals, IntExp},
		serialize_list,
	},
	IntVal, MetaInfo,
};

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Precedence<Identifier = String> {
	pub info: MetaInfo<Identifier>,
	pub list: Vec<IntExp<Identifier>>,
	pub values: Vec<IntVal>,
	pub covered: bool,
}

impl<'de, Identifier: FromStr> Deserialize<'de> for Precedence<Identifier> {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		#[derive(Default, Deserialize)]
		struct Values {
			#[serde(default, rename = "@covered")]
			covered: Option<bool>,
			#[serde(rename = "$text", deserialize_with = "deserialize_int_vals")]
			list: Vec<IntVal>,
		}
		#[derive(Deserialize)]
		#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
		pub struct Precedence<Identifier = String> {
			#[serde(flatten)]
			pub info: MetaInfo<Identifier>,
			#[serde(
				alias = "$text",
				deserialize_with = "deserialize_int_exps",
				serialize_with = "serialize_list"
			)]
			pub list: Vec<IntExp<Identifier>>,
			#[serde(default)]
			pub values: Values,
		}
		let x = Precedence::deserialize(deserializer)?;
		Ok(Self {
			info: x.info,
			list: x.list,
			values: x.values.list,
			covered: x.values.covered.unwrap_or(false),
		})
	}
}

impl<Identifier: Display> Serialize for Precedence<Identifier> {
	fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		#[derive(Serialize)]
		struct Values<'a> {
			#[serde(rename = "@covered", skip_serializing_if = "is_false")]
			covered: bool,
			#[serde(rename = "$text", serialize_with = "serialize_list")]
			list: &'a Vec<IntVal>,
		}
		impl Values<'_> {
			fn can_skip(&self) -> bool {
				!self.covered && self.list.is_empty()
			}
		}
		#[derive(Serialize)]
		#[serde(bound(serialize = "Identifier: Display"))]
		pub struct Precedence<'a, Identifier = String> {
			#[serde(flatten)]
			pub info: &'a MetaInfo<Identifier>,
			#[serde(alias = "$text", serialize_with = "serialize_list")]
			pub list: &'a Vec<IntExp<Identifier>>,
			#[serde(skip_serializing_if = "Values::can_skip")]
			pub values: Values<'a>,
		}
		let x = Precedence {
			info: &self.info,
			list: &self.list,
			values: Values {
				covered: self.covered,
				list: &self.values,
			},
		};
		x.serialize(serializer)
	}
}

fn is_false(x: &bool) -> bool {
	!x
}
