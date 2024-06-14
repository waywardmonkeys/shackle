pub(crate) mod all_different;
pub(crate) mod all_equal;
pub(crate) mod bin_packing;
pub(crate) mod channel;
pub(crate) mod circuit;
pub(crate) mod cumulative;
pub(crate) mod element;
pub(crate) mod extension;
pub(crate) mod instantiation;
pub(crate) mod intension;
pub(crate) mod knapsack;
pub(crate) mod maximum;
pub(crate) mod minimum;
pub(crate) mod no_overlap;
pub(crate) mod ordered;
pub(crate) mod precedence;

use std::{fmt::Display, marker::PhantomData, str::FromStr};

use nom::{
	branch::alt,
	bytes::complete::tag,
	character::streaming::char,
	sequence::{delimited, separated_pair},
};
use serde::{de::Visitor, Deserialize, Deserializer, Serialize, Serializer};

use crate::{
	constraint::{
		all_different::AllDifferent, all_equal::AllEqual, bin_packing::BinPacking,
		channel::Channel, circuit::Circuit, cumulative::Cumulative, element::Element,
		extension::Extension, instantiation::Instantiation, intension::Intension,
		knapsack::Knapsack, maximum::Maximum, minimum::Minimum, no_overlap::NoOverlap,
		ordered::Ordered, precedence::Precedence,
	},
	parser::integer::{int_exp, IntExp},
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
	#[serde(rename = "channel")]
	Channel(Channel<Identifier>),
	#[serde(rename = "circuit")]
	Circuit(Circuit<Identifier>),
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
	#[serde(rename = "minimum")]
	Minimum(Minimum<Identifier>),
	#[serde(rename = "noOverlap")]
	NoOverlap(NoOverlap<Identifier>),
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
		}
	}
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Condition<Identifier> {
	pub operator: Operator,
	pub operand: IntExp<Identifier>,
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
						)),
						char(','),
						int_exp,
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
