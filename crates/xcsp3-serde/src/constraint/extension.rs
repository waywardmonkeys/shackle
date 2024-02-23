use core::panic;
use std::{borrow::Cow, fmt::Display, str::FromStr};

use itertools::Itertools;
use nom::combinator::all_consuming;
use serde::{Deserialize, Serialize};

use crate::{
	parser::{identifier::variable, integer::int, sequence, tuple, whitespace_seperated},
	variable::VarRef,
	IntVal,
};

// TODO: Support for "smart" extension

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Extension<Identifier = String> {
	list: Vec<VarRef<Identifier>>,
	supports: Vec<Vec<IntVal>>,
	conflicts: Vec<Vec<IntVal>>,
}

impl<'de, Identifier: FromStr> Deserialize<'de> for Extension<Identifier> {
	fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		#[derive(Deserialize)]
		struct E<'a> {
			list: Cow<'a, str>,
			supports: Option<Cow<'a, str>>,
			conflicts: Option<Cow<'a, str>>,
		}
		let x: E = Deserialize::deserialize(deserializer)?;
		let list = match all_consuming(whitespace_seperated(variable))(&x.list) {
			Ok((_, li)) => li,
			Err(err) => panic!("{} -> {}", x.list, err),
		};
		let supports = if let Some(supports) = x.supports {
			match all_consuming(sequence(tuple(int)))(&supports) {
				Ok((_, li)) => li,
				Err(err) => panic!("{}", err),
			}
		} else {
			Vec::new()
		};
		let conflicts = if let Some(conflicts) = x.conflicts {
			match all_consuming(sequence(tuple(int)))(&conflicts) {
				Ok((_, li)) => li,
				Err(_) => todo!(),
			}
		} else {
			Vec::new()
		};

		Ok(Self {
			list,
			supports,
			conflicts,
		})
	}
}

impl<Identifier: Display> Serialize for Extension<Identifier> {
	fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		#[derive(Serialize)]
		struct E<'a> {
			list: &'a str,
			supports: Option<&'a str>,
			conflicts: Option<&'a str>,
		}

		let output_tuples = |x: &Vec<Vec<IntVal>>| {
			format!(
				"{}",
				x.iter().format_with("", |tup, f| {
					f(&format_args!("({})", tup.iter().format(",")))
				})
			)
		};

		let sup_str = output_tuples(&self.supports);
		let conf_str = output_tuples(&self.conflicts);

		Serialize::serialize(
			&E {
				list: &format!("{}", self.list.iter().format(" ")),
				supports: if self.supports.is_empty() {
					None
				} else {
					Some(&sup_str)
				},
				conflicts: if self.conflicts.is_empty() {
					None
				} else {
					Some(&conf_str)
				},
			},
			serializer,
		)
	}
}
