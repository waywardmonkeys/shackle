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
pub(crate) mod instantiation;
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
		cumulative::Cumulative, element::Element, extension::Extension,
		instantiation::Instantiation, intension::Intension, knapsack::Knapsack, maximum::Maximum,
		mdd::Mdd, minimum::Minimum, n_values::NValues, no_overlap::NoOverlap, ordered::Ordered,
		precedence::Precedence, regular::Regular, sum::Sum,
	},
	parser::{exp, identifier::identifier, integer::int, sequence, Exp},
	IntVal,
};

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub enum Constraint<Identifier = String> {
	#[serde(rename = "allDifferent")]
	AllDifferent(AllDifferent<Identifier>),
	#[serde(rename = "allEqual")]
	AllEqual(AllEqual<Identifier>),
	#[serde(rename = "binPacking")]
	BinPacking(BinPacking<Identifier>),
	#[serde(rename = "cardinality")]
	Cardinality(Cardinality<Identifier>),
	#[serde(rename = "channel")]
	Channel(Channel<Identifier>),
	#[serde(rename = "circuit")]
	Circuit(Circuit<Identifier>),
	#[serde(rename = "count")]
	Count(Count<Identifier>),
	#[serde(rename = "cumulative")]
	Cumulative(Cumulative<Identifier>),
	#[serde(rename = "element")]
	Element(Element<Identifier>),
	#[serde(rename = "extension")]
	Extension(Extension<Identifier>),
	#[serde(rename = "instantiation")]
	Instantiation(Instantiation<Identifier>),
	#[serde(rename = "intension")]
	Intension(Intension<Identifier>),
	#[serde(rename = "knapsack")]
	Knapsack(Knapsack<Identifier>),
	#[serde(rename = "maximum")]
	Maximum(Maximum<Identifier>),
	#[serde(rename = "mdd")]
	Mdd(Mdd<Identifier>),
	#[serde(rename = "minimum")]
	Minimum(Minimum<Identifier>),
	#[serde(rename = "nValues")]
	NValues(NValues<Identifier>),
	#[serde(rename = "noOverlap")]
	NoOverlap(NoOverlap<Identifier>),
	#[serde(rename = "ordered")]
	Ordered(Ordered<Identifier>),
	#[serde(rename = "precedence")]
	Precedence(Precedence<Identifier>),
	#[serde(rename = "regular")]
	Regular(Regular<Identifier>),
	#[serde(rename = "sum")]
	Sum(Sum<Identifier>),
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
pub enum Operator {
	#[serde(rename = "lt")]
	Lt,
	#[serde(rename = "le")]
	Le,
	#[serde(rename = "eq")]
	Eq,
	#[serde(rename = "ge")]
	Ge,
	#[serde(rename = "gt")]
	Gt,
	#[serde(rename = "ne")]
	Ne,
	#[serde(rename = "in")]
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
