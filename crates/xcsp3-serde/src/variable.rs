use std::{
	borrow::{Borrow, Cow},
	fmt::{self, Display},
	marker::PhantomData,
	str::FromStr,
};

use flatzinc_serde::RangeList;
use itertools::Itertools;
use nom::combinator::all_consuming;
use serde::{
	de::{self, Visitor},
	Deserialize, Deserializer, Serialize, Serializer,
};

use crate::{
	parser::{identifier::variable, integer::range, whitespace_seperated},
	IntVal,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Variable<Identifier = String> {
	identifier: Identifier,
	domain: RangeList<IntVal>,
}

impl<'de, Identifier: Deserialize<'de>> Deserialize<'de> for Variable<Identifier> {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		#[derive(Deserialize)]
		#[serde(rename = "var")]
		pub struct Var<'a, Identifier = String> {
			#[serde(rename = "@id")]
			identifier: Identifier,
			#[serde(rename = "$value")]
			domain: Cow<'a, str>,
		}
		let v: Var<Identifier> = Deserialize::deserialize(deserializer)?;
		let res = match all_consuming(whitespace_seperated(range))(&v.domain) {
			Ok((_, mut r)) => {
				r.sort_by_key(|i| *i.start());
				let mut it = r.into_iter();
				let mut ranges = Vec::new();
				let mut cur = it.next().unwrap();
				for next in it {
					if *cur.end() >= (next.start() - 1) {
						cur = *cur.start()..=*next.end()
					} else {
						ranges.push(cur);
						cur = next;
					}
				}
				ranges.push(cur);
				ranges.into_iter().collect()
			}
			Err(err) => return Err(de::Error::custom(err.borrow())),
		};
		Ok(Self {
			identifier: v.identifier,
			domain: res,
		})
	}
}

impl<Identifier: Serialize> Serialize for Variable<Identifier> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		#[derive(Serialize)]
		pub struct Var<'a, Identifier = String> {
			#[serde(rename = "@id")]
			identifier: &'a Identifier,
			#[serde(rename = "$value")]
			domain: &'a str,
		}

		Serialize::serialize(
			&Var {
				identifier: &self.identifier,
				domain: &format!(
					"{}",
					(&self.domain).into_iter().format_with(" ", |r, f| {
						if r.start() == r.end() {
							f(&format_args!("{}", *r.start()))
						} else {
							f(&format_args!("{}..{}", *r.start(), *r.end()))
						}
					})
				),
			},
			serializer,
		)
	}
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum VarRef<Identifier> {
	Ident(Identifier),
	ArrayAccess(Identifier, Vec<Option<IntVal>>),
}

impl<Identifier: Display> Display for VarRef<Identifier> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			VarRef::Ident(ident) => ident.fmt(f),
			VarRef::ArrayAccess(ident, v) => {
				write!(
					f,
					"{}{}",
					ident,
					v.iter().format_with("", |el, f| f(&format_args!(
						"[{}]",
						el.map_or_else(String::new, |i| i.to_string())
					)))
				)
			}
		}
	}
}

pub(crate) fn deserialize_var_refs<'de, D: Deserializer<'de>, Identifier: FromStr>(
	deserializer: D,
) -> Result<Vec<VarRef<Identifier>>, D::Error> {
	struct V<X>(PhantomData<X>);
	impl<'de, X: FromStr> Visitor<'de> for V<X> {
		type Value = Vec<VarRef<X>>;
		fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
			formatter.write_str("a list of variable references")
		}
		fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
			let (_, v) = all_consuming(whitespace_seperated(variable))(v)
				.map_err(|e| E::custom(format!("invalid variable references {e:?}")))?;
			Ok(v)
		}
	}
	let visitor = V::<Identifier>(PhantomData);
	deserializer.deserialize_str(visitor)
}

pub(crate) fn serialize_var_refs<S: serde::Serializer, Identifier: Display>(
	exps: &[VarRef<Identifier>],
	serializer: S,
) -> Result<S::Ok, S::Error> {
	serializer.serialize_str(
		&exps
			.iter()
			.map(|e| format!("{}", e))
			.collect::<Vec<_>>()
			.join(" "),
	)
}
