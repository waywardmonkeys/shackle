use std::{fmt::Display, str::FromStr};

use nom::{
	branch::alt,
	character::complete::{char, multispace0, multispace1},
	combinator::map,
	error::ParseError,
	multi::{many0, separated_list1},
	sequence::{delimited, preceded, terminated},
	IResult,
};

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
		map(bool_exp, |x| Exp::Bool(Box::new(x))),
		map(int_exp, |x| Exp::Int(Box::new(x))),
		map(set_exp, |x| Exp::Set(Box::new(x))),
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
	delimited(char('('), separated_list1(ws(char(',')), p), char(')'))
}

/// Parser combinator that repeatedly calls a parser consuming whitespace in
/// between when possible
pub fn sequence<'a, O>(
	p: impl FnMut(&'a str) -> IResult<&'a str, O>,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>> {
	terminated(many0(preceded(multispace0, p)), multispace0)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
pub fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(
	inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
	F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
	delimited(multispace0, inner, multispace0)
}
