use std::{borrow::Cow, fmt::Display, str::FromStr};

use nom::combinator::all_consuming;
use serde::{de, Deserialize, Deserializer, Serialize, Serializer};

use crate::{
	parser::{
		integer::{int_exp, IntExp},
		whitespace_seperated,
	},
	MetaInfo,
};

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Channel<Identifier = String> {
	pub info: MetaInfo<Identifier>,
	pub list: Vec<IntExp<Identifier>>,
	pub inverse_list: Vec<IntExp<Identifier>>,
	pub value: Option<IntExp<Identifier>>,
}

impl<'de, Identifier: FromStr> Deserialize<'de> for Channel<Identifier> {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Channel<Identifier>, D::Error> {
		#[derive(Deserialize)]
		#[serde(bound(deserialize = "I: FromStr"))]
		struct Channel<'a, I: FromStr> {
			#[serde(flatten)]
			info: MetaInfo<I>,
			list: Vec<Cow<'a, str>>,
			#[serde(default)]
			value: Option<IntExp<I>>,
		}
		let c = Channel::deserialize(deserializer)?;
		if c.list.is_empty() {
			return Err(de::Error::missing_field("list"));
		}
		let (_, list) = all_consuming(whitespace_seperated(int_exp))(&c.list[0])
			.map_err(|e| de::Error::custom(format!("invalid integer expressions {e:?}")))?;
		let inverse_list = if let Some(inverse_list) = c.list.get(1) {
			all_consuming(whitespace_seperated(int_exp))(inverse_list)
				.map_err(|e| de::Error::custom(format!("invalid integer expressions {e:?}")))?
				.1
		} else {
			Vec::new()
		};

		Ok(Self {
			info: c.info,
			list,
			inverse_list,
			value: c.value,
		})
	}
}

impl<Identifier: Display> Serialize for Channel<Identifier> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		#[derive(Serialize)]
		#[serde(bound(serialize = "I: Display"))]
		struct Channel<'a, I: Display> {
			#[serde(flatten)]
			info: &'a MetaInfo<I>,
			list: Vec<String>,
			#[serde(skip_serializing_if = "Option::is_none")]
			value: &'a Option<IntExp<I>>,
		}

		let p = |i: &Vec<IntExp<Identifier>>| -> String {
			i.iter()
				.map(|e| format!("{}", e))
				.collect::<Vec<_>>()
				.join(" ")
		};

		let mut c = Channel {
			info: &self.info,
			list: vec![p(&self.list)],
			value: &self.value,
		};
		if !self.inverse_list.is_empty() {
			c.list.push(p(&self.inverse_list))
		}
		c.serialize(serializer)
	}
}
