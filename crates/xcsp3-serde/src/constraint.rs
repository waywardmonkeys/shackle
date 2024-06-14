pub(crate) mod all_different;
pub(crate) mod all_equal;
pub(crate) mod circuit;
pub(crate) mod extension;
pub(crate) mod instantiation;
pub(crate) mod intension;
pub(crate) mod ordered;
pub(crate) mod precedence;

use std::{fmt::Display, marker::PhantomData, str::FromStr};

use serde::{de::Visitor, Deserialize, Deserializer, Serialize};

use crate::constraint::{
	all_different::AllDifferent, all_equal::AllEqual, circuit::Circuit, extension::Extension,
	instantiation::Instantiation, intension::Intension, ordered::Ordered, precedence::Precedence,
};

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub enum Constraint<Identifier = String> {
	#[serde(rename = "allDifferent")]
	AllDifferent(AllDifferent<Identifier>),
	#[serde(rename = "allEqual")]
	AllEqual(AllEqual<Identifier>),
	#[serde(rename = "circuit")]
	Circuit(Circuit<Identifier>),
	#[serde(rename = "extension")]
	Extension(Extension<Identifier>),
	#[serde(rename = "instantiation")]
	Instantiation(Instantiation<Identifier>),
	#[serde(rename = "intension")]
	Intension(Intension<Identifier>),
	#[serde(rename = "ordered")]
	Ordered(Ordered<Identifier>),
	#[serde(rename = "precedence")]
	Precedence(Precedence<Identifier>),
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct ConstraintMeta<Identifier> {
	#[serde(
		default,
		rename = "@id",
		deserialize_with = "deserialize_ident",
		skip_serializing_if = "Option::is_none",
		serialize_with = "serialize_ident"
	)]
	pub id: Option<Identifier>,
	#[serde(rename = "@note", skip_serializing_if = "String::is_empty", default)]
	pub note: String,
}

fn deserialize_ident<'de, D: Deserializer<'de>, Identifier: FromStr>(
	deserializer: D,
) -> Result<Option<Identifier>, D::Error> {
	struct V<X>(PhantomData<X>);
	impl<'de, X: FromStr> Visitor<'de> for V<X> {
		type Value = Option<X>;
		fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
			formatter.write_str("an identfier")
		}
		fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
			Ok(Some(FromStr::from_str(v).map_err(|_| {
				E::custom("unable to create identifier from string")
			})?))
		}
	}
	let visitor = V::<Identifier>(PhantomData);
	deserializer.deserialize_str(visitor)
}

fn serialize_ident<S: serde::Serializer, Identifier: Display>(
	identifier: &Option<Identifier>,
	serializer: S,
) -> Result<S::Ok, S::Error> {
	serializer.serialize_str(&format!("{}", identifier.as_ref().unwrap()))
}
