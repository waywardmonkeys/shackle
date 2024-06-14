use std::{fmt::Display, marker::PhantomData, ops::RangeInclusive, str::FromStr};

use nom::{
	branch::alt,
	bytes::complete::tag,
	character::complete::{char, digit1},
	combinator::{all_consuming, map, map_res, opt, recognize},
	multi::separated_list1,
	IResult,
};
use serde::{de::Visitor, Deserializer};

use super::{
	boolean::{bool_exp, BoolExp},
	exp,
	identifier::variable,
	set::{set_exp, SetExp},
	Exp,
};
use crate::{
	parser::{sequence, tuple, whitespace_seperated},
	variable::VarRef,
	IntVal,
};

pub fn range(input: &str) -> IResult<&str, RangeInclusive<IntVal>> {
	let (input, lb) = int(input)?;
	if let (input, Some(_)) = opt(tag(".."))(input)? {
		let (input, ub) = int(input)?;
		Ok((input, lb..=ub))
	} else {
		Ok((input, lb..=lb))
	}
}

pub fn int(input: &str) -> IResult<&str, IntVal> {
	let (input, neg) = opt(char('-'))(input)?;
	let (input, i): (_, i64) = map_res(recognize(digit1), str::parse)(input)?;
	Ok((input, if neg.is_some() { -i } else { i }))
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub enum IntExp<Identifier> {
	Const(IntVal),
	Var(VarRef<Identifier>),
	Neg(Box<IntExp<Identifier>>),
	Abs(Box<IntExp<Identifier>>),
	Add(Vec<IntExp<Identifier>>),
	Sub(Box<IntExp<Identifier>>, Box<IntExp<Identifier>>),
	Mul(Vec<IntExp<Identifier>>),
	Div(Box<IntExp<Identifier>>, Box<IntExp<Identifier>>),
	Mod(Box<IntExp<Identifier>>, Box<IntExp<Identifier>>),
	Sqrt(Box<IntExp<Identifier>>),
	Pow(Box<IntExp<Identifier>>, Box<IntExp<Identifier>>),
	Min(Vec<Exp<Identifier>>),
	Max(Vec<Exp<Identifier>>),
	Dist(Box<IntExp<Identifier>>, Box<IntExp<Identifier>>),
	If(
		BoolExp<Identifier>,
		Box<IntExp<Identifier>>,
		Box<IntExp<Identifier>>,
	),
	Bool(BoolExp<Identifier>),
	Card(Box<SetExp<Identifier>>),
}

impl<Identifier: Display> Display for IntExp<Identifier> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			IntExp::Const(i) => write!(f, "{}", i),
			IntExp::Var(id) => write!(f, "{}", id),
			IntExp::Neg(e) => write!(f, "neg({})", e),
			IntExp::Abs(e) => write!(f, "abs({})", e),
			IntExp::Add(es) => write!(
				f,
				"add({})",
				es.iter()
					.map(|e| e.to_string())
					.collect::<Vec<_>>()
					.join(",")
			),
			IntExp::Sub(e1, e2) => write!(f, "sub({},{})", e1, e2),
			IntExp::Mul(es) => write!(
				f,
				"mul({})",
				es.iter()
					.map(|e| e.to_string())
					.collect::<Vec<_>>()
					.join(",")
			),
			IntExp::Div(e1, e2) => write!(f, "div({},{})", e1, e2),
			IntExp::Mod(e1, e2) => write!(f, "mod({},{})", e1, e2),
			IntExp::Sqrt(e) => write!(f, "sqr({})", e),
			IntExp::Pow(e1, e2) => write!(f, "pow({},{})", e1, e2),
			IntExp::Min(es) => write!(
				f,
				"min({})",
				es.iter()
					.map(|e| e.to_string())
					.collect::<Vec<_>>()
					.join(",")
			),
			IntExp::Max(es) => write!(
				f,
				"max({})",
				es.iter()
					.map(|e| e.to_string())
					.collect::<Vec<_>>()
					.join(",")
			),
			IntExp::Dist(e1, e2) => write!(f, "dist({},{})", e1, e2),
			IntExp::If(b, e1, e2) => write!(f, "if({},{},{})", b, e1, e2),
			IntExp::Bool(b) => write!(f, "{}", b),
			IntExp::Card(s) => write!(f, "card({})", s),
		}
	}
}

pub fn int_exp<Identifier: FromStr>(input: &str) -> IResult<&str, IntExp<Identifier>> {
	alt((
		map(int, IntExp::Const),
		one_arg,
		one_arg_set,
		two_arg,
		three_arg,
		var_arg,
		var_arg_exp,
		map(variable, IntExp::Var),
		map(bool_exp, IntExp::Bool),
	))(input)
}

fn one_arg<Identifier: FromStr>(input: &str) -> IResult<&str, IntExp<Identifier>> {
	let (input, tag) = alt((tag("neg"), tag("abs"), tag("sqrt"), tag("sqr")))(input)?;
	let (input, _) = char('(')(input)?;
	let (input, e) = int_exp(input)?;
	let (input, _) = char(')')(input)?;
	Ok((
		input,
		match tag {
			"neg" => IntExp::Neg,
			"abs" => IntExp::Abs,
			"sqr" | "sqrt" => IntExp::Sqrt,
			_ => unreachable!(),
		}(Box::new(e)),
	))
}

fn one_arg_set<Identifier: FromStr>(input: &str) -> IResult<&str, IntExp<Identifier>> {
	let (input, tag) = tag("card")(input)?;
	let (input, _) = char('(')(input)?;
	let (input, e) = set_exp(input)?;
	let (input, _) = char(')')(input)?;
	Ok((
		input,
		match tag {
			"card" => IntExp::Card,
			_ => unreachable!(),
		}(Box::new(e)),
	))
}

fn two_arg<Identifier: FromStr>(input: &str) -> IResult<&str, IntExp<Identifier>> {
	let (input, tag) = alt((tag("sub"), tag("div"), tag("mod"), tag("pow"), tag("dist")))(input)?;
	let (input, _) = char('(')(input)?;
	let (input, e1) = int_exp(input)?;
	let (input, _) = char(',')(input)?;
	let (input, e2) = int_exp(input)?;
	let (input, _) = char(')')(input)?;
	Ok((
		input,
		match tag {
			"sub" => IntExp::Sub,
			"div" => IntExp::Div,
			"mod" => IntExp::Mod,
			"pow" => IntExp::Pow,
			"dist" => IntExp::Dist,
			_ => unreachable!(),
		}(Box::new(e1), Box::new(e2)),
	))
}

fn three_arg<Identifier: FromStr>(input: &str) -> IResult<&str, IntExp<Identifier>> {
	let (input, tag) = alt((tag("if"),))(input)?;
	let (input, _) = char('(')(input)?;
	let (input, e1) = bool_exp(input)?;
	let (input, _) = char(',')(input)?;
	let (input, e2) = int_exp(input)?;
	let (input, _) = char(',')(input)?;
	let (input, e3) = int_exp(input)?;
	let (input, _) = char(')')(input)?;
	Ok((
		input,
		match tag {
			"if" => IntExp::If,
			_ => unreachable!(),
		}(e1, Box::new(e2), Box::new(e3)),
	))
}

fn var_arg<Identifier: FromStr>(input: &str) -> IResult<&str, IntExp<Identifier>> {
	let (input, tag) = alt((tag("add"), tag("mul")))(input)?;
	let (input, _) = char('(')(input)?;
	let (input, es) = separated_list1(char(','), int_exp)(input)?;
	let (input, _) = char(')')(input)?;
	Ok((
		input,
		match tag {
			"add" => IntExp::Add,
			"mul" => IntExp::Mul,
			_ => unreachable!(),
		}(es),
	))
}

fn var_arg_exp<Identifier: FromStr>(input: &str) -> IResult<&str, IntExp<Identifier>> {
	let (input, tag) = alt((tag("min"), tag("max")))(input)?;
	let (input, _) = char('(')(input)?;
	let (input, es) = separated_list1(char(','), exp)(input)?;
	let (input, _) = char(')')(input)?;
	Ok((
		input,
		match tag {
			"min" => IntExp::Min,
			"max" => IntExp::Max,
			_ => unreachable!(),
		}(es),
	))
}

pub fn deserialize_int_exps<'de, D: Deserializer<'de>, Identifier: FromStr>(
	deserializer: D,
) -> Result<Vec<IntExp<Identifier>>, D::Error> {
	struct V<X>(PhantomData<X>);
	impl<'de, X: FromStr> Visitor<'de> for V<X> {
		type Value = Vec<IntExp<X>>;
		fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
			formatter.write_str("a list of integers expressions")
		}
		fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
			let (_, v) = all_consuming(whitespace_seperated(int_exp))(v)
				.map_err(|e| E::custom(format!("invalid integer expressions {e:?}")))?;
			Ok(v)
		}
	}
	let visitor = V::<Identifier>(PhantomData);
	deserializer.deserialize_str(visitor)
}

pub fn serialize_int_exps<S: serde::Serializer, Identifier: Display>(
	exps: &[IntExp<Identifier>],
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

pub fn deserialize_int_vals<'de, D: Deserializer<'de>>(
	deserializer: D,
) -> Result<Vec<IntVal>, D::Error> {
	struct V;
	impl<'de> Visitor<'de> for V {
		type Value = Vec<IntVal>;
		fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
			formatter.write_str("a list of integers")
		}
		fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
			let (_, v) = all_consuming(whitespace_seperated(int))(v)
				.map_err(|e| E::custom(format!("invalid list of integers {e:?}")))?;
			Ok(v)
		}
	}
	deserializer.deserialize_str(V)
}

pub fn serialize_int_vals<S: serde::Serializer>(
	vals: &[IntVal],
	serializer: S,
) -> Result<S::Ok, S::Error> {
	serializer.serialize_str(
		&vals
			.iter()
			.map(|e| format!("{}", e))
			.collect::<Vec<_>>()
			.join(" "),
	)
}

pub fn deserialize_int_tuples<'de, D: serde::Deserializer<'de>>(
	deserializer: D,
) -> Result<Vec<Vec<IntVal>>, D::Error> {
	struct V;
	impl<'de> Visitor<'de> for V {
		type Value = Vec<Vec<IntVal>>;
		fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
			formatter.write_str("an integer")
		}
		fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
			let (_, v) = all_consuming(sequence(tuple(int)))(v)
				.map_err(|e| E::custom(format!("invalid integer {e:?}")))?;
			Ok(v)
		}
	}
	deserializer.deserialize_str(V)
}

pub fn serialize_int_tuples<S: serde::Serializer>(
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
