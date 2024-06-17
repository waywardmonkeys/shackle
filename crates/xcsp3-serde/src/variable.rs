use std::{
	borrow::Cow,
	fmt::{self, Display},
	marker::PhantomData,
	ops::RangeInclusive,
	str::FromStr,
};

use flatzinc_serde::RangeList;
use nom::{
	character::complete::{char, digit1},
	combinator::{all_consuming, map_res, recognize},
	sequence::delimited,
};
use serde::{de::Visitor, Deserialize, Deserializer, Serialize};

use crate::{
	parser::{
		identifier::{deserialize_from_str, serialize_as_str, variable},
		integer::range,
		sequence, serialize_list, whitespace_seperated,
	},
	IntVal,
};

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Array<Identifier = String> {
	pub identifier: Identifier,
	pub note: Option<String>,
	pub size: Vec<usize>,
	pub domains: Vec<(Vec<VarRef<Identifier>>, RangeList<IntVal>)>,
}

impl<'de, Identifier: Deserialize<'de> + FromStr> Deserialize<'de> for Array<Identifier> {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		#[derive(Deserialize)]
		struct DomainStruct<Identifier: FromStr> {
			#[serde(rename = "@for", deserialize_with = "deserialize_var_refs")]
			vars: Vec<VarRef<Identifier>>,
			#[serde(rename = "$text", deserialize_with = "deserialize_range_list")]
			domain: RangeList<IntVal>,
		}
		#[derive(Deserialize)]
		enum Domain<'a, Identifier: FromStr> {
			#[serde(rename = "domain")]
			Domain(Vec<DomainStruct<Identifier>>),
			#[serde(rename = "$text")]
			Direct(Cow<'a, str>),
		}
		#[derive(Deserialize)]
		struct Array<'a, Identifier: FromStr> {
			#[serde(rename = "@id", deserialize_with = "deserialize_from_str")]
			identifier: Identifier,
			#[serde(rename = "@note", default, skip_serializing_if = "Option::is_none")]
			note: Option<String>,
			#[serde(rename = "@size", deserialize_with = "deserialize_size")]
			size: Vec<usize>,
			#[serde(rename = "$value")]
			domain: Domain<'a, Identifier>,
		}
		let x = Array::deserialize(deserializer)?;
		let domains = match x.domain {
			Domain::Domain(v) => v.into_iter().map(|d| (d.vars, d.domain)).collect(),
			Domain::Direct(s) => {
				let s = all_consuming(whitespace_seperated(range))(s.as_ref())
					.map_err(serde::de::Error::custom)?;
				vec![(
					vec![VarRef::Ident(
						Identifier::from_str("others").unwrap_or_else(|_| {
							panic!("unable to create identifier from `\"others\"`")
						}),
					)],
					collect_range_list(s.1),
				)]
			}
		};
		Ok(Self {
			identifier: x.identifier,
			note: x.note,
			size: x.size,
			domains,
		})
	}
}

impl<Identifier: Serialize + Display> Serialize for Array<Identifier> {
	fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		#[derive(Serialize)]
		struct DomainStruct<'a, Identifier: Display> {
			#[serde(rename = "@for", serialize_with = "serialize_list")]
			vars: &'a Vec<VarRef<Identifier>>,
			#[serde(rename = "$text", serialize_with = "serialize_range_list")]
			domain: &'a RangeList<IntVal>,
		}
		#[derive(Serialize)]
		enum Domain<'a, Identifier: Display> {
			#[serde(rename = "domain")]
			Domain(DomainStruct<'a, Identifier>),
		}
		#[derive(Serialize)]
		struct Array<'a, Identifier: Display> {
			#[serde(rename = "@id", serialize_with = "serialize_as_str")]
			identifier: &'a Identifier,
			#[serde(rename = "@note", default, skip_serializing_if = "Option::is_none")]
			note: &'a Option<String>,
			#[serde(rename = "@size", serialize_with = "serialize_size")]
			size: &'a Vec<usize>,
			#[serde(rename = "$value")]
			domain: Vec<Domain<'a, Identifier>>,
		}
		let domain = self
			.domains
			.iter()
			.map(|(v, d)| Domain::Domain(DomainStruct { vars: v, domain: d }))
			.collect();
		let x = Array {
			identifier: &self.identifier,
			note: &self.note,
			size: &self.size,
			domain,
		};
		x.serialize(serializer)
	}
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Variable<Identifier = String> {
	#[serde(
		rename = "@id",
		deserialize_with = "deserialize_from_str",
		serialize_with = "serialize_as_str"
	)]
	pub identifier: Identifier,
	#[serde(rename = "@note", default, skip_serializing_if = "Option::is_none")]
	pub note: Option<String>,
	#[serde(
		rename = "$text",
		deserialize_with = "deserialize_range_list",
		serialize_with = "serialize_range_list"
	)]
	pub domain: RangeList<IntVal>,
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum Index {
	/// Accessing a single index of a dimension in an array
	Single(IntVal),
	/// Accessing a slice of a dimension in an array
	Range(IntVal, IntVal),
	/// Accessing the full range of an array
	Full,
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum VarRef<Identifier> {
	Ident(Identifier),
	ArrayAccess(Identifier, Vec<Index>),
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
					v.iter()
						.map(|i| format!(
							"[{}]",
							match i {
								Index::Single(v) => v.to_string(),
								Index::Range(a, b) => format!("{}..{}", a, b),
								Index::Full => String::new(),
							}
						))
						.collect::<Vec<_>>()
						.join("")
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

fn deserialize_range_list<'de, D: Deserializer<'de>>(
	deserializer: D,
) -> Result<RangeList<IntVal>, D::Error> {
	struct V;
	impl<'de> Visitor<'de> for V {
		type Value = RangeList<IntVal>;
		fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
			formatter.write_str("a list of ranges")
		}
		fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
			let (_, r) = all_consuming(whitespace_seperated(range))(v)
				.map_err(|e| E::custom(format!("invalid list of ranges {e:?}")))?;
			Ok(collect_range_list(r))
		}
	}
	let visitor = V;
	deserializer.deserialize_str(visitor)
}
fn serialize_range_list<S: serde::Serializer>(
	exps: &RangeList<IntVal>,
	serializer: S,
) -> Result<S::Ok, S::Error> {
	serializer.serialize_str(
		&exps
			.into_iter()
			.map(|e| {
				if e.start() == e.end() {
					e.start().to_string()
				} else {
					format!("{}..{}", e.start(), e.end())
				}
			})
			.collect::<Vec<_>>()
			.join(" "),
	)
}

fn collect_range_list<I: IntoIterator<Item = RangeInclusive<IntVal>>>(
	iter: I,
) -> RangeList<IntVal> {
	let mut r: Vec<_> = iter.into_iter().collect();
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

fn deserialize_size<'de, D: Deserializer<'de>>(deserializer: D) -> Result<Vec<usize>, D::Error> {
	struct V;
	impl<'de> Visitor<'de> for V {
		type Value = Vec<usize>;
		fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
			formatter.write_str("an array size expression")
		}
		fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
			let (_, r) = all_consuming(sequence(delimited(
				char('['),
				map_res(recognize(digit1), str::parse),
				char(']'),
			)))(v)
			.map_err(|e| E::custom(format!("invalid array size expression {e:?}")))?;
			Ok(r)
		}
	}
	let visitor = V;
	deserializer.deserialize_str(visitor)
}

fn serialize_size<S: serde::Serializer>(exps: &[usize], serializer: S) -> Result<S::Ok, S::Error> {
	serializer.serialize_str(
		&exps
			.iter()
			.map(|e| format!("[{}]", e))
			.collect::<Vec<_>>()
			.join(""),
	)
}
