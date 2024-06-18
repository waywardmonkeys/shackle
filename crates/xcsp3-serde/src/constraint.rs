pub(crate) mod all_different;
pub(crate) mod all_equal;
pub(crate) mod bin_packing;
pub(crate) mod cardinality;
pub(crate) mod channel;
pub(crate) mod circuit;
pub(crate) mod count;
pub(crate) mod cumulative;
pub(crate) mod element;
pub(crate) mod extension;
pub(crate) mod intension;
pub(crate) mod knapsack;
pub(crate) mod maximum;
pub(crate) mod mdd;
pub(crate) mod minimum;
pub(crate) mod n_values;
pub(crate) mod no_overlap;
pub(crate) mod ordered;
pub(crate) mod precedence;
pub(crate) mod regular;
pub(crate) mod sum;

use std::{fmt::Display, marker::PhantomData, str::FromStr};

use nom::{
	branch::alt,
	bytes::complete::tag,
	character::complete::char,
	combinator::{all_consuming, map},
	sequence::{delimited, separated_pair, tuple},
};
use serde::{de::Visitor, Deserialize, Deserializer, Serialize, Serializer};

use crate::{
	constraint::{
		all_different::AllDifferent, all_equal::AllEqual, bin_packing::BinPacking,
		cardinality::Cardinality, channel::Channel, circuit::Circuit, count::Count,
		cumulative::Cumulative, element::Element, extension::Extension, intension::Intension,
		knapsack::Knapsack, maximum::Maximum, mdd::Mdd, minimum::Minimum, n_values::NValues,
		no_overlap::NoOverlap, ordered::Ordered, precedence::Precedence, regular::Regular,
		sum::Sum,
	},
	parser::{
		exp,
		identifier::identifier,
		integer::{deserialize_int_exps, int, IntExp},
		sequence, serialize_list, Exp,
	},
	Instantiation, IntVal,
};

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(
	rename_all = "camelCase",
	bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display")
)]
pub enum Constraint<Identifier = String> {
	AllDifferent(AllDifferent<Identifier>),
	AllEqual(AllEqual<Identifier>),
	BinPacking(BinPacking<Identifier>),
	Cardinality(Cardinality<Identifier>),
	Channel(Channel<Identifier>),
	Circuit(Circuit<Identifier>),
	Count(Count<Identifier>),
	Cumulative(Cumulative<Identifier>),
	Element(Element<Identifier>),
	Extension(Extension<Identifier>),
	Instantiation(Instantiation<Identifier>),
	Intension(Intension<Identifier>),
	Knapsack(Knapsack<Identifier>),
	Maximum(Maximum<Identifier>),
	Mdd(Mdd<Identifier>),
	Minimum(Minimum<Identifier>),
	NValues(NValues<Identifier>),
	NoOverlap(NoOverlap<Identifier>),
	Ordered(Ordered<Identifier>),
	Precedence(Precedence<Identifier>),
	Regular(Regular<Identifier>),
	Sum(Sum<Identifier>),
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum Operator {
	Lt,
	Le,
	Eq,
	Ge,
	Gt,
	Ne,
	In,
}

impl Display for Operator {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Operator::Lt => write!(f, "lt"),
			Operator::Le => write!(f, "le"),
			Operator::Eq => write!(f, "eq"),
			Operator::Ge => write!(f, "ge"),
			Operator::Gt => write!(f, "gt"),
			Operator::Ne => write!(f, "ne"),
			Operator::In => write!(f, "in"),
		}
	}
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Condition<Identifier> {
	pub operator: Operator,
	pub operand: Exp<Identifier>,
}

impl<Identifier: Display> Display for Condition<Identifier> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "({},{})", self.operator, self.operand)
	}
}

impl<'de, Identifier: FromStr> Deserialize<'de> for Condition<Identifier> {
	fn deserialize<D: Deserializer<'de>>(
		deserializer: D,
	) -> Result<Condition<Identifier>, D::Error> {
		struct V<X>(PhantomData<X>);
		impl<'de, X: FromStr> Visitor<'de> for V<X> {
			type Value = Condition<X>;
			fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
				formatter.write_str("a condition")
			}
			fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
				let mut parser = delimited(
					char('('),
					separated_pair(
						alt((
							tag("lt"),
							tag("le"),
							tag("eq"),
							tag("ge"),
							tag("gt"),
							tag("ne"),
							tag("in"),
						)),
						char(','),
						exp,
					),
					char(')'),
				);
				let (_, (operator, operand)) =
					parser(v).map_err(|e| E::custom(format!("invalid condition {e:?}")))?;
				let operator = match operator {
					"lt" => Operator::Lt,
					"le" => Operator::Le,
					"eq" => Operator::Eq,
					"ge" => Operator::Ge,
					"gt" => Operator::Gt,
					"ne" => Operator::Ne,
					"in" => Operator::In,
					_ => unreachable!(),
				};
				Ok(Condition { operator, operand })
			}
		}
		deserializer.deserialize_str(V(PhantomData::<Identifier>))
	}
}

impl<Identifier: Display> Serialize for Condition<Identifier> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		serializer.serialize_str(&self.to_string())
	}
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Transition<Identifier> {
	pub from: Identifier,
	pub val: IntVal,
	pub to: Identifier,
}

impl<Identifier: Display> Display for Transition<Identifier> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "({},{},{})", self.from, self.val, self.to)
	}
}

pub(crate) fn deserialize_transitions<'de, D: Deserializer<'de>, Identifier: FromStr>(
	deserializer: D,
) -> Result<Vec<Transition<Identifier>>, D::Error> {
	struct V<X>(PhantomData<X>);
	impl<'de, X: FromStr> Visitor<'de> for V<X> {
		type Value = Vec<Transition<X>>;
		fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
			formatter.write_str("a list of transitions")
		}
		fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
			eprintln!("{}", v);
			let transition = map(
				tuple((
					char('('),
					identifier,
					char(','),
					int,
					char(','),
					identifier,
					char(')'),
				)),
				|(_, from, _, val, _, to, _)| Transition { from, val, to },
			);
			let (_, v) = all_consuming(sequence(transition))(v)
				.map_err(|e| E::custom(format!("invalid transitions {e:?}")))?;
			Ok(v)
		}
	}
	let visitor = V::<Identifier>(PhantomData);
	deserializer.deserialize_str(visitor)
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct OffsetList<Identifier> {
	#[serde(
		alias = "$text",
		deserialize_with = "deserialize_int_exps",
		serialize_with = "serialize_list"
	)]
	list: Vec<IntExp<Identifier>>,
	#[serde(rename = "@startIndex", default, skip_serializing_if = "is_default")]
	start_index: IntVal,
}

impl<Identifier> Default for OffsetList<Identifier> {
	fn default() -> Self {
		Self {
			list: Vec::new(),
			start_index: IntVal::default(),
		}
	}
}

pub(crate) fn is_default<T: Default + PartialEq>(val: &T) -> bool {
	val == &T::default()
}
