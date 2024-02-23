use std::{
	borrow::{Borrow, Cow},
	fmt::{self, Display},
};

use flatzinc_serde::RangeList;
use itertools::Itertools;
use nom::combinator::all_consuming;
use serde::{de, Deserialize, Deserializer, Serialize, Serializer};

use crate::{
	parser::{integer::range, whitespace_seperated},
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
	ArrayAccess(Identifier, IntVal),
}

impl<Identifier: Display> Display for VarRef<Identifier> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			VarRef::Ident(ident) => ident.fmt(f),
			VarRef::ArrayAccess(ident, i) => write!(f, "{}[{}]", ident, i),
		}
	}
}
