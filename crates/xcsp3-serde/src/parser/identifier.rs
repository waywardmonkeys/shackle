use std::{borrow::Cow, fmt::Display, marker::PhantomData, str::FromStr};

use nom::{
	character::complete::{alpha1, alphanumeric0, char},
	combinator::{opt, recognize, verify},
	multi::many0,
	sequence::{delimited, tuple},
	IResult,
};
use serde::{de::Visitor, Deserialize, Deserializer, Serializer};

use crate::{parser::integer::int, variable::VarRef};

const RESERVED: &[&str] = &[
	"abs", "add", "and", "card", "convex", "diff", "disjoint", "dist", "div", "eq", "ge", "gt",
	"hull", "if", "iff", "imp", "in", "inter", "le", "lt", "max", "min", "mod", "mul", "ne", "neg",
	"not", "or", "pow", "sdiff", "set", "sqr", "sqrt", "sub", "subseq", "subset", "superseq",
	"superset", "union", "xor",
];

pub fn identifier<Identifier: FromStr>(input: &str) -> IResult<&str, Identifier> {
	let (input, v) = verify(recognize(tuple((alpha1, alphanumeric0))), |s: &str| {
		!RESERVED.contains(&s)
	})(input)?;
	Ok((
		input,
		match v.parse() {
			Ok(t) => t,
			Err(_) => panic!("unable to create identifier"),
		},
	))
}

pub fn variable<Identifier: FromStr>(input: &str) -> IResult<&str, VarRef<Identifier>> {
	let (input, ident) = identifier(input)?;
	let (input, v) = many0(delimited(char('['), opt(int), char(']')))(input)?;
	Ok((
		input,
		if v.is_empty() {
			VarRef::Ident(ident)
		} else {
			VarRef::ArrayAccess(ident, v)
		},
	))
}

pub(crate) fn deserialize_ident<'de, D: Deserializer<'de>, Identifier: FromStr>(
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

pub(crate) fn serialize_ident<S: serde::Serializer, Identifier: Display>(
	identifier: &Option<Identifier>,
	serializer: S,
) -> Result<S::Ok, S::Error> {
	serializer.serialize_str(&format!("{}", identifier.as_ref().unwrap()))
}

pub(crate) fn deserialize_from_str<'de, D: Deserializer<'de>, I: FromStr>(
	deserializer: D,
) -> Result<I, D::Error> {
	let s: Cow<'_, str> = Deserialize::deserialize(deserializer)?;
	match s.parse() {
		Ok(t) => Ok(t),
		Err(_) => Err(serde::de::Error::custom("unable to parse from string")),
	}
}

pub(crate) fn serialize_as_str<S: Serializer, I: Display>(
	value: &I,
	serializer: S,
) -> Result<S::Ok, S::Error> {
	serializer.serialize_str(&value.to_string())
}
