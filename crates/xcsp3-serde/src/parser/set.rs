use std::{fmt::Display, str::FromStr};

use nom::{
	branch::alt,
	bytes::complete::tag,
	character::complete::char,
	combinator::map,
	multi::{separated_list0, separated_list1},
	sequence::{delimited, pair, separated_pair},
	IResult,
};

use super::{
	identifier::variable,
	integer::{int_exp, IntExp},
};
use crate::variable::VarRef;

#[derive(Clone, Debug, PartialEq, Hash)]
pub enum SetExp<Identifier> {
	Var(VarRef<Identifier>),
	Set(Vec<IntExp<Identifier>>),
	Range((IntExp<Identifier>, IntExp<Identifier>)),
	Hull(Box<SetExp<Identifier>>),
	Diff(Box<SetExp<Identifier>>, Box<SetExp<Identifier>>),
	Union(Vec<SetExp<Identifier>>),
	Inter(Vec<SetExp<Identifier>>),
	SDiff(Vec<SetExp<Identifier>>),
}

impl<Identifier: Display> Display for SetExp<Identifier> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			SetExp::Var(id) => write!(f, "{}", id),
			SetExp::Set(es) => write!(
				f,
				"{{{}}}",
				es.iter()
					.map(|e| e.to_string())
					.collect::<Vec<_>>()
					.join(",")
			),
			SetExp::Range((from, to)) => write!(f, "{}..{}", from, to),
			SetExp::Hull(e) => write!(f, "hull({})", e),
			SetExp::Diff(e1, e2) => write!(f, "diff({}, {})", e1, e2),
			SetExp::Union(es) => write!(
				f,
				"union({})",
				es.iter()
					.map(|e| e.to_string())
					.collect::<Vec<_>>()
					.join(",")
			),
			SetExp::Inter(es) => write!(
				f,
				"inter({})",
				es.iter()
					.map(|e| e.to_string())
					.collect::<Vec<_>>()
					.join(",")
			),
			SetExp::SDiff(es) => write!(
				f,
				"sdiff({})",
				es.iter()
					.map(|e| e.to_string())
					.collect::<Vec<_>>()
					.join(",")
			),
		}
	}
}

pub fn set_exp<Identifier: FromStr>(input: &str) -> IResult<&str, SetExp<Identifier>> {
	alt((
		map(separated_pair(int_exp, tag(".."), int_exp), |(from, to)| {
			SetExp::Range((from, to))
		}),
		one_arg,
		two_arg,
		var_arg,
		map(
			delimited(
				pair(tag("set"), char('(')),
				separated_list0(char(','), int_exp),
				char(')'),
			),
			SetExp::Set,
		),
		map(variable, SetExp::Var),
	))(input)
}

fn one_arg<Identifier: FromStr>(input: &str) -> IResult<&str, SetExp<Identifier>> {
	let (input, tag) = tag("hull")(input)?;
	let (input, _) = char('(')(input)?;
	let (input, e) = set_exp(input)?;
	let (input, _) = char(')')(input)?;
	Ok((
		input,
		match tag {
			"hull" => SetExp::Hull,
			_ => unreachable!(),
		}(Box::new(e)),
	))
}

fn two_arg<Identifier: FromStr>(input: &str) -> IResult<&str, SetExp<Identifier>> {
	let (input, tag) = tag("diff")(input)?;
	let (input, _) = char('(')(input)?;
	let (input, e1) = set_exp(input)?;
	let (input, _) = char(',')(input)?;
	let (input, e2) = set_exp(input)?;
	let (input, _) = char(')')(input)?;
	Ok((
		input,
		match tag {
			"diff" => SetExp::Diff,
			_ => unreachable!(),
		}(Box::new(e1), Box::new(e2)),
	))
}

fn var_arg<Identifier: FromStr>(input: &str) -> IResult<&str, SetExp<Identifier>> {
	let (input, tag) = alt((tag("union"), tag("inter"), tag("sdiff")))(input)?;
	let (input, _) = char('(')(input)?;
	let (input, es) = separated_list1(char(','), set_exp)(input)?;
	let (input, _) = char(')')(input)?;
	Ok((
		input,
		match tag {
			"union" => SetExp::Union,
			"inter" => SetExp::Inter,
			"sdiff" => SetExp::SDiff,
			_ => unreachable!(),
		}(es),
	))
}
