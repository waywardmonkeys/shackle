use std::{borrow::Cow, fmt::Display, marker::PhantomData, str::FromStr};

use nom::{
	branch::alt,
	bytes::complete::tag,
	character::complete::char,
	combinator::{all_consuming, map},
	sequence::{delimited, separated_pair},
};
use serde::{
	de::{self, Visitor},
	Deserialize, Deserializer, Serialize, Serializer,
};

use crate::{
	as_str, deserialize_int_vals,
	expression::{identifier, int, sequence, tuple, whitespace_seperated, BoolExp, Exp, IntExp},
	from_str, serialize_list, Instantiation, IntVal, MetaInfo,
};

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct AllDifferent<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(
		alias = "$text",
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub list: Vec<IntExp<Identifier>>,
	#[serde(
		default,
		skip_serializing_if = "Vec::is_empty",
		deserialize_with = "deserialize_int_vals",
		serialize_with = "serialize_list"
	)]
	pub except: Vec<IntVal>,
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct AllEqual<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(
		alias = "$text",
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub list: Vec<IntExp<Identifier>>,
	#[serde(
		default,
		skip_serializing_if = "Vec::is_empty",
		deserialize_with = "deserialize_int_vals",
		serialize_with = "serialize_list"
	)]
	pub except: Vec<IntVal>,
}

// TODO: Should `condition`, `limits` and `loads` be made mutually exclusive in the struct?
#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(
	deserialize = "Identifier: std::str::FromStr",
	serialize = "Identifier: std::fmt::Display"
))]
pub struct BinPacking<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub list: Vec<IntExp<Identifier>>,
	#[serde(
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub sizes: Vec<IntExp<Identifier>>,
	#[serde(default, skip_serializing_if = "Option::is_none")]
	pub condition: Option<Condition<Identifier>>,
	#[serde(
		default,
		skip_serializing_if = "Vec::is_empty",
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub limits: Vec<IntExp<Identifier>>,
	#[serde(
		default,
		skip_serializing_if = "Vec::is_empty",
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub loads: Vec<IntExp<Identifier>>,
}

#[derive(Clone, Debug, PartialEq, Hash)]

pub struct Cardinality<Identifier = String> {
	pub info: MetaInfo<Identifier>,
	pub list: Vec<IntExp<Identifier>>,
	pub values: Vec<IntExp<Identifier>>,
	pub closed: bool,
	pub occurs: Vec<Exp<Identifier>>,
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Channel<Identifier = String> {
	pub info: MetaInfo<Identifier>,
	pub list: Vec<IntExp<Identifier>>,
	pub inverse_list: Vec<IntExp<Identifier>>,
	pub value: Option<IntExp<Identifier>>,
}

#[derive(Clone, Debug, PartialEq, Hash, Serialize)]
#[serde(bound(serialize = "Identifier: Display"))]
pub struct Circuit<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	pub list: OffsetList<Identifier>,
	#[serde(skip_serializing_if = "is_exp_zero")]
	pub size: IntExp<Identifier>,
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Condition<Identifier> {
	pub operator: Operator,
	pub operand: Exp<Identifier>,
}

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
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Count<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub list: Vec<IntExp<Identifier>>,
	#[serde(
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub values: Vec<IntExp<Identifier>>,
	pub condition: Condition<Identifier>,
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Cumulative<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub origins: Vec<IntExp<Identifier>>,
	#[serde(
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub lengths: Vec<IntExp<Identifier>>,
	#[serde(
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub heights: Vec<IntExp<Identifier>>,
	pub condition: Condition<Identifier>,
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Element<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	pub list: OffsetList<Identifier>,
	#[serde(default, skip_serializing_if = "Option::is_none")]
	pub index: Option<IntExp<Identifier>>,
	#[serde(default, skip_serializing_if = "Option::is_none")]
	pub value: Option<IntExp<Identifier>>,
	#[serde(default, skip_serializing_if = "Option::is_none")]
	pub condition: Option<Condition<Identifier>>,
}

// TODO: Support for "smart" extension
#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Extension<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(
		alias = "$text",
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub list: Vec<IntExp<Identifier>>,
	#[serde(
		default,
		skip_serializing_if = "Vec::is_empty",
		deserialize_with = "deserialize_int_tuples",
		serialize_with = "serialize_int_tuples"
	)]
	pub supports: Vec<Vec<IntVal>>,
	#[serde(
		default,
		skip_serializing_if = "Vec::is_empty",
		deserialize_with = "deserialize_int_tuples",
		serialize_with = "serialize_int_tuples"
	)]
	pub conflicts: Vec<Vec<IntVal>>,
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Intension<Identifier> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(alias = "$text")]
	pub function: BoolExp<Identifier>,
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Knapsack<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub list: Vec<IntExp<Identifier>>,
	#[serde(
		deserialize_with = "deserialize_int_vals",
		serialize_with = "serialize_list"
	)]
	pub weights: Vec<IntVal>,
	#[serde(
		deserialize_with = "deserialize_int_vals",
		serialize_with = "serialize_list"
	)]
	pub profits: Vec<IntVal>,
	/// The first `Condition` element is related to weights whereas the second [`Condition`] element is related to profits.
	pub condition: [Condition<Identifier>; 2],
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Maximum<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(
		alias = "$text",
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub list: Vec<IntExp<Identifier>>,
	pub condition: Condition<Identifier>,
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Mdd<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub list: Vec<IntExp<Identifier>>,
	#[serde(
		deserialize_with = "Transition::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub transitions: Vec<Transition<Identifier>>,
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Minimum<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(
		alias = "$text",
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub list: Vec<IntExp<Identifier>>,
	pub condition: Condition<Identifier>,
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct NValues<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub list: Vec<IntExp<Identifier>>,
	#[serde(
		default,
		skip_serializing_if = "Vec::is_empty",
		deserialize_with = "deserialize_int_vals",
		serialize_with = "serialize_list"
	)]
	pub except: Vec<IntVal>,
	pub condition: Condition<Identifier>,
}

// TODO: k-dimensional no-overlap constraint
#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct NoOverlap<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(
		default = "bool_true",
		skip_serializing_if = "is_true",
		rename = "@zeroIgnored"
	)]
	pub zero_ignored: bool,
	#[serde(
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub origins: Vec<IntExp<Identifier>>,
	#[serde(
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub lengths: Vec<IntExp<Identifier>>,
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct OffsetList<Identifier> {
	#[serde(
		alias = "$text",
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	list: Vec<IntExp<Identifier>>,
	#[serde(rename = "@startIndex", default, skip_serializing_if = "is_default")]
	start_index: IntVal,
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

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Ordered<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(
		alias = "$text",
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub list: Vec<IntExp<Identifier>>,
	#[serde(
		default,
		skip_serializing_if = "Vec::is_empty",
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub lengths: Vec<IntExp<Identifier>>,
	pub operator: Operator,
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Precedence<Identifier = String> {
	pub info: MetaInfo<Identifier>,
	pub list: Vec<IntExp<Identifier>>,
	pub values: Vec<IntVal>,
	pub covered: bool,
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Regular<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub list: Vec<IntExp<Identifier>>,
	#[serde(
		deserialize_with = "Transition::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub transitions: Vec<Transition<Identifier>>,
	#[serde(deserialize_with = "from_str", serialize_with = "as_str")]
	pub start: Identifier,
	#[serde(
		rename = "final",
		deserialize_with = "from_str",
		serialize_with = "as_str"
	)]
	pub finish: Identifier,
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Sum<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub list: Vec<IntExp<Identifier>>,
	#[serde(
		default,
		skip_serializing_if = "Vec::is_empty",
		deserialize_with = "deserialize_int_vals",
		serialize_with = "serialize_list"
	)]
	pub coeffs: Vec<IntVal>,
	pub condition: Condition<Identifier>,
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Transition<Identifier> {
	pub from: Identifier,
	pub val: IntVal,
	pub to: Identifier,
}

fn bool_true() -> bool {
	true
}

fn deserialize_int_tuples<'de, D: Deserializer<'de>>(
	deserializer: D,
) -> Result<Vec<Vec<IntVal>>, D::Error> {
	struct V;
	impl<'de> Visitor<'de> for V {
		type Value = Vec<Vec<IntVal>>;

		fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
			formatter.write_str("an integer")
		}

		fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
			let (_, v) = all_consuming(sequence(tuple(int)))(v)
				.map_err(|e| E::custom(format!("invalid integer {e:?}")))?;
			Ok(v)
		}
	}
	deserializer.deserialize_str(V)
}

fn exp_zero<I>() -> IntExp<I> {
	IntExp::Const(0)
}

fn is_default<T: Default + PartialEq>(val: &T) -> bool {
	val == &T::default()
}

fn is_exp_zero<I>(val: &IntExp<I>) -> bool {
	matches!(val, IntExp::Const(0))
}

fn is_false(x: &bool) -> bool {
	!x
}

fn is_true(x: &bool) -> bool {
	*x
}

fn serialize_int_tuples<S: Serializer>(
	vals: &[Vec<IntVal>],
	serializer: S,
) -> Result<S::Ok, S::Error> {
	serializer.serialize_str(
		&vals
			.iter()
			.map(|e| {
				format!(
					"({})",
					e.iter()
						.map(|e| format!("{}", e))
						.collect::<Vec<_>>()
						.join(",")
				)
			})
			.collect::<Vec<_>>()
			.join(""),
	)
}

impl<'de, Identifier: FromStr> Deserialize<'de> for Cardinality<Identifier> {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		#[derive(Deserialize)]
		#[serde(bound(deserialize = "Identifier: FromStr"))]
		struct Values<Identifier> {
			#[serde(default, rename = "@closed")]
			closed: Option<bool>,
			#[serde(rename = "$text", deserialize_with = "IntExp::parse_vec")]
			list: Vec<IntExp<Identifier>>,
		}
		#[derive(Deserialize)]
		#[serde(bound(deserialize = "Identifier: FromStr"))]
		struct Cardinality<Identifier = String> {
			#[serde(flatten)]
			info: MetaInfo<Identifier>,
			#[serde(deserialize_with = "IntExp::parse_vec")]
			list: Vec<IntExp<Identifier>>,
			values: Values<Identifier>,
			#[serde(deserialize_with = "Exp::parse_vec")]
			occurs: Vec<Exp<Identifier>>,
		}
		let x = Cardinality::deserialize(deserializer)?;
		Ok(Self {
			info: x.info,
			list: x.list,
			values: x.values.list,
			closed: x.values.closed.unwrap_or(false),
			occurs: x.occurs,
		})
	}
}

impl<Identifier: Display> Serialize for Cardinality<Identifier> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		#[derive(Serialize)]
		#[serde(bound(serialize = "Identifier: Display"))]
		struct Values<'a, Identifier> {
			#[serde(rename = "@closed", skip_serializing_if = "is_false")]
			closed: bool,
			#[serde(rename = "$text", serialize_with = "serialize_list")]
			list: &'a Vec<IntExp<Identifier>>,
		}
		#[derive(Serialize)]
		#[serde(bound(serialize = "Identifier: Display"))]
		struct Cardinality<'a, Identifier = String> {
			#[serde(flatten)]
			info: &'a MetaInfo<Identifier>,
			#[serde(serialize_with = "serialize_list")]
			list: &'a Vec<IntExp<Identifier>>,
			values: Values<'a, Identifier>,
			#[serde(serialize_with = "serialize_list")]
			occurs: &'a Vec<Exp<Identifier>>,
		}
		let x = Cardinality {
			info: &self.info,
			list: &self.list,
			values: Values {
				closed: self.closed,
				list: &self.values,
			},
			occurs: &self.occurs,
		};
		x.serialize(serializer)
	}
}

impl<'de, Identifier: FromStr> Deserialize<'de> for Channel<Identifier> {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Channel<Identifier>, D::Error> {
		#[derive(Deserialize)]
		#[serde(bound(deserialize = "I: FromStr"))]
		struct Channel<'a, I: FromStr> {
			#[serde(flatten)]
			info: MetaInfo<I>,
			list: Vec<Cow<'a, str>>,
			#[serde(default)]
			value: Option<IntExp<I>>,
		}
		let c = Channel::deserialize(deserializer)?;
		if c.list.is_empty() {
			return Err(de::Error::missing_field("list"));
		}
		let (_, list) = all_consuming(whitespace_seperated(IntExp::parse))(&c.list[0])
			.map_err(|e| de::Error::custom(format!("invalid integer expressions {e:?}")))?;
		let inverse_list = if let Some(inverse_list) = c.list.get(1) {
			all_consuming(whitespace_seperated(IntExp::parse))(inverse_list)
				.map_err(|e| de::Error::custom(format!("invalid integer expressions {e:?}")))?
				.1
		} else {
			Vec::new()
		};

		Ok(Self {
			info: c.info,
			list,
			inverse_list,
			value: c.value,
		})
	}
}

impl<Identifier: Display> Serialize for Channel<Identifier> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		#[derive(Serialize)]
		#[serde(bound(serialize = "I: Display"))]
		struct Channel<'a, I: Display> {
			#[serde(flatten)]
			info: &'a MetaInfo<I>,
			list: Vec<String>,
			#[serde(skip_serializing_if = "Option::is_none")]
			value: &'a Option<IntExp<I>>,
		}

		let p = |i: &Vec<IntExp<Identifier>>| -> String {
			i.iter()
				.map(|e| format!("{}", e))
				.collect::<Vec<_>>()
				.join(" ")
		};

		let mut c = Channel {
			info: &self.info,
			list: vec![p(&self.list)],
			value: &self.value,
		};
		if !self.inverse_list.is_empty() {
			c.list.push(p(&self.inverse_list))
		}
		c.serialize(serializer)
	}
}

impl<'de, Identifier: FromStr> Deserialize<'de> for Circuit<Identifier> {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		#[derive(Deserialize)]
		#[serde(bound = "Identifier: FromStr")]
		struct Circuit<Identifier> {
			#[serde(flatten)]
			info: MetaInfo<Identifier>,
			#[serde(default, alias = "$text")]
			simple: Vec<IntExp<Identifier>>,
			#[serde(default)]
			list: OffsetList<Identifier>,
			#[serde(default = "exp_zero")]
			size: IntExp<Identifier>,
		}
		let mut x = Circuit::deserialize(deserializer)?;
		if !x.simple.is_empty() {
			x.list = OffsetList {
				list: x.simple,
				start_index: 0,
			};
		}
		Ok(Self {
			info: x.info,
			list: x.list,
			size: x.size,
		})
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

			fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
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
						Exp::parse,
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

impl<Identifier: Display> Display for Condition<Identifier> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "({},{})", self.operator, self.operand)
	}
}

impl<Identifier: Display> Serialize for Condition<Identifier> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		serializer.serialize_str(&self.to_string())
	}
}

impl<Identifier> Default for OffsetList<Identifier> {
	fn default() -> Self {
		Self {
			list: Vec::new(),
			start_index: IntVal::default(),
		}
	}
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

impl<'de, Identifier: FromStr> Deserialize<'de> for Precedence<Identifier> {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		#[derive(Default, Deserialize)]
		struct Values {
			#[serde(default, rename = "@covered")]
			covered: Option<bool>,
			#[serde(rename = "$text", deserialize_with = "deserialize_int_vals")]
			list: Vec<IntVal>,
		}
		#[derive(Deserialize)]
		#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
		struct Precedence<Identifier = String> {
			#[serde(flatten)]
			info: MetaInfo<Identifier>,
			#[serde(
				alias = "$text",
				deserialize_with = "IntExp::parse_vec",
				serialize_with = "serialize_list"
			)]
			list: Vec<IntExp<Identifier>>,
			#[serde(default)]
			values: Values,
		}
		let x = Precedence::deserialize(deserializer)?;
		Ok(Self {
			info: x.info,
			list: x.list,
			values: x.values.list,
			covered: x.values.covered.unwrap_or(false),
		})
	}
}

impl<Identifier: Display> Serialize for Precedence<Identifier> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		#[derive(Serialize)]
		struct Values<'a> {
			#[serde(rename = "@covered", skip_serializing_if = "is_false")]
			covered: bool,
			#[serde(rename = "$text", serialize_with = "serialize_list")]
			list: &'a Vec<IntVal>,
		}
		impl Values<'_> {
			fn can_skip(&self) -> bool {
				!self.covered && self.list.is_empty()
			}
		}
		#[derive(Serialize)]
		#[serde(bound(serialize = "Identifier: Display"))]
		struct Precedence<'a, Identifier = String> {
			#[serde(flatten)]
			info: &'a MetaInfo<Identifier>,
			#[serde(alias = "$text", serialize_with = "serialize_list")]
			list: &'a Vec<IntExp<Identifier>>,
			#[serde(skip_serializing_if = "Values::can_skip")]
			values: Values<'a>,
		}
		let x = Precedence {
			info: &self.info,
			list: &self.list,
			values: Values {
				covered: self.covered,
				list: &self.values,
			},
		};
		x.serialize(serializer)
	}
}

impl<Identifier: FromStr> Transition<Identifier> {
	fn parse_vec<'de, D: Deserializer<'de>>(deserializer: D) -> Result<Vec<Self>, D::Error> {
		struct V<X>(PhantomData<X>);
		impl<'de, X: FromStr> Visitor<'de> for V<X> {
			type Value = Vec<Transition<X>>;

			fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
				formatter.write_str("a list of transitions")
			}

			fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
				eprintln!("{}", v);
				let transition = map(
					nom::sequence::tuple((
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
}

impl<Identifier: Display> Display for Transition<Identifier> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "({},{},{})", self.from, self.val, self.to)
	}
}
