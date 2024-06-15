use std::{fmt::Display, marker::PhantomData, str::FromStr};

use nom::{
	branch::alt,
	character::complete::{char, multispace0, multispace1},
	combinator::{all_consuming, map},
	multi::{many0, separated_list1},
	sequence::{delimited, preceded, terminated},
	IResult,
};
use serde::{de::Visitor, Deserializer, Serializer};

use self::{
	boolean::{bool_exp, BoolExp},
	identifier::variable,
	integer::{int_exp, IntExp},
	set::{set_exp, SetExp},
};
use crate::variable::VarRef;

pub(crate) mod boolean;
pub(crate) mod identifier;
pub(crate) mod integer;
pub(crate) mod set;

#[derive(Clone, Debug, PartialEq, Hash)]
pub enum Exp<Identifier> {
	Bool(Box<BoolExp<Identifier>>),
	Int(Box<IntExp<Identifier>>),
	Set(Box<SetExp<Identifier>>),
	Var(VarRef<Identifier>),
}

impl<Identifier: Display> Display for Exp<Identifier> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Exp::Bool(e) => write!(f, "{}", e),
			Exp::Int(e) => write!(f, "{}", e),
			Exp::Set(e) => write!(f, "{}", e),
			Exp::Var(e) => write!(f, "{}", e),
		}
	}
}

pub fn exp<Identifier: FromStr>(input: &str) -> IResult<&str, Exp<Identifier>> {
	alt((
		map(variable, |x| Exp::Var(x)),
		map(set_exp, |x| Exp::Set(Box::new(x))),
		map(bool_exp, |x| Exp::Bool(Box::new(x))),
		map(int_exp, |x| Exp::Int(Box::new(x))),
	))(input)
}

/// Parser combinator that requires whitespace between parsing rules
pub fn whitespace_seperated<'a, O>(
	p: impl FnMut(&'a str) -> IResult<&'a str, O>,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>> {
	separated_list1(multispace1, p)
}

/// Parser combinator that expects parentheses with a comma seperated parser
/// rules
pub fn tuple<'a, O>(
	p: impl FnMut(&'a str) -> IResult<&'a str, O>,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>> {
	delimited(char('('), separated_list1(char(','), p), char(')'))
}

/// Parser combinator that repeatedly calls a parser consuming whitespace in
/// between when possible
pub fn sequence<'a, O>(
	p: impl FnMut(&'a str) -> IResult<&'a str, O>,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>> {
	terminated(many0(preceded(multispace0, p)), multispace0)
}

pub(crate) fn deserialize_exps<'de, D: Deserializer<'de>, Identifier: FromStr>(
	deserializer: D,
) -> Result<Vec<Exp<Identifier>>, D::Error> {
	struct V<X>(PhantomData<X>);
	impl<'de, X: FromStr> Visitor<'de> for V<X> {
		type Value = Vec<Exp<X>>;
		fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
			formatter.write_str("a list of expressions")
		}
		fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
			let (_, v) = all_consuming(whitespace_seperated(exp))(v)
				.map_err(|e| E::custom(format!("invalid expressions {e:?}")))?;
			Ok(v)
		}
	}
	let visitor = V::<Identifier>(PhantomData);
	deserializer.deserialize_str(visitor)
}

pub(crate) fn serialize_list<S: Serializer, T: Display>(
	exps: &[T],
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
