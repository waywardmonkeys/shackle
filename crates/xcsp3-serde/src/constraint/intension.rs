use std::{borrow::Cow, fmt::Display, str::FromStr};

use nom::combinator::all_consuming;
use serde::{Deserialize, Serialize};

use crate::parser::boolean::{bool_exp, BoolExp};

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Intension<Identifier> {
	id: Option<Identifier>,
	function: BoolExp<Identifier>,
}

impl<'de, Identifier: FromStr> Deserialize<'de> for Intension<Identifier> {
	fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		#[derive(Deserialize)]
		pub struct Intension<'a> {
			#[serde(rename = "@id")]
			identifier: Option<Cow<'a, str>>,
			#[serde(alias = "$value")]
			function: Cow<'a, str>,
		}

		let tmp = Intension::deserialize(deserializer)?;
		let (_, function) =
			all_consuming(bool_exp)(tmp.function.as_ref()).map_err(serde::de::Error::custom)?;
		let mut id = None;
		if let Some(ident) = tmp.identifier {
			id = Some(FromStr::from_str(ident.as_ref()).map_err(|_| {
				serde::de::Error::custom("unable to create identifier from string")
			})?);
		}
		Ok(Self { id, function })
	}
}

impl<Identifier: Display> Serialize for Intension<Identifier> {
	fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		#[derive(Serialize)]
		struct Intension<'a> {
			#[serde(rename = "@id", skip_serializing_if = "Option::is_none")]
			identifier: Option<String>,
			function: &'a str,
		}
		let tmp = Intension {
			identifier: self.id.as_ref().map(|id| id.to_string()),
			function: &self.function.to_string(),
		};
		tmp.serialize(serializer)
	}
}
