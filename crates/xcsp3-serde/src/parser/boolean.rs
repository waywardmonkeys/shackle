use std::{fmt::Display, marker::PhantomData, str::FromStr};

use nom::{
	branch::alt,
	bytes::complete::tag,
	character::complete::{char, digit1},
	combinator::{map, verify},
	multi::{separated_list0, separated_list1},
	IResult,
};
use serde::{de::Visitor, Deserialize, Deserializer, Serialize, Serializer};

use super::{
	exp,
	identifier::variable,
	integer::{int_exp, IntExp},
	set::{set_exp, SetExp},
	Exp,
};
use crate::variable::VarRef;

#[derive(Clone, Debug, PartialEq, Hash)]
pub enum BoolExp<Identifier> {
	Const(bool),
	Var(VarRef<Identifier>),
	Not(Box<BoolExp<Identifier>>),
	And(Vec<BoolExp<Identifier>>),
	Or(Vec<BoolExp<Identifier>>),
	Xor(Vec<BoolExp<Identifier>>),
	Equiv(Vec<BoolExp<Identifier>>),
	Implies(Box<BoolExp<Identifier>>, Box<BoolExp<Identifier>>),
	LessThan(Box<IntExp<Identifier>>, Box<IntExp<Identifier>>),
	LessThanEq(Box<IntExp<Identifier>>, Box<IntExp<Identifier>>),
	GreaterThan(Box<IntExp<Identifier>>, Box<IntExp<Identifier>>),
	GreaterThanEq(Box<IntExp<Identifier>>, Box<IntExp<Identifier>>),
	NotEqual(Box<Exp<Identifier>>, Box<Exp<Identifier>>),
	Equal(Vec<Exp<Identifier>>),
	Member(Box<IntExp<Identifier>>, Box<SetExp<Identifier>>),
	Disjoint(Box<SetExp<Identifier>>, Box<SetExp<Identifier>>),
	SubSet(Box<SetExp<Identifier>>, Box<SetExp<Identifier>>),
	SubSetEq(Box<SetExp<Identifier>>, Box<SetExp<Identifier>>),
	SuperSet(Box<SetExp<Identifier>>, Box<SetExp<Identifier>>),
	SuperSetEq(Box<SetExp<Identifier>>, Box<SetExp<Identifier>>),
	Convex(Box<SetExp<Identifier>>),
}

impl<Identifier: Display> Display for BoolExp<Identifier> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			BoolExp::Const(b) => write!(f, "{}", if *b { 1 } else { 0 }),
			BoolExp::Var(id) => write!(f, "{}", id),
			BoolExp::Not(e) => write!(f, "not({})", e),
			BoolExp::And(es) => write!(
				f,
				"and({})",
				es.iter()
					.map(|e| e.to_string())
					.collect::<Vec<_>>()
					.join(",")
			),
			BoolExp::Or(es) => write!(
				f,
				"or({})",
				es.iter()
					.map(|e| e.to_string())
					.collect::<Vec<_>>()
					.join(",")
			),
			BoolExp::Xor(es) => write!(
				f,
				"xor({})",
				es.iter()
					.map(|e| e.to_string())
					.collect::<Vec<_>>()
					.join(",")
			),
			BoolExp::Equiv(es) => write!(
				f,
				"iff({})",
				es.iter()
					.map(|e| e.to_string())
					.collect::<Vec<_>>()
					.join(",")
			),
			BoolExp::Implies(e1, e2) => write!(f, "imp({},{})", e1, e2),
			BoolExp::LessThan(e1, e2) => write!(f, "lt({},{})", e1, e2),
			BoolExp::LessThanEq(e1, e2) => write!(f, "le({},{})", e1, e2),
			BoolExp::GreaterThan(e1, e2) => write!(f, "gt({},{})", e1, e2),
			BoolExp::GreaterThanEq(e1, e2) => write!(f, "ge({},{})", e1, e2),
			BoolExp::NotEqual(e1, e2) => write!(f, "ne({},{})", e1, e2),
			BoolExp::Equal(es) => write!(
				f,
				"eq({})",
				es.iter()
					.map(|e| e.to_string())
					.collect::<Vec<_>>()
					.join(",")
			),
			BoolExp::Member(e1, e2) => write!(f, "in({},{})", e1, e2),
			BoolExp::Disjoint(e1, e2) => write!(f, "disjoint({},{})", e1, e2),
			BoolExp::SubSet(e1, e2) => write!(f, "subset({},{})", e1, e2),
			BoolExp::SubSetEq(e1, e2) => write!(f, "subseq({},{})", e1, e2),
			BoolExp::SuperSet(e1, e2) => write!(f, "supset({},{})", e1, e2),
			BoolExp::SuperSetEq(e1, e2) => write!(f, "supseq({},{})", e1, e2),
			BoolExp::Convex(e) => write!(f, "convex({})", e),
		}
	}
}

impl<'de, Identifier: FromStr> Deserialize<'de> for BoolExp<Identifier> {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<BoolExp<Identifier>, D::Error> {
		struct V<Ident>(PhantomData<Ident>);
		impl<'de, Ident: FromStr> Visitor<'de> for V<Ident> {
			type Value = BoolExp<Ident>;
			fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
				formatter.write_str("an integer")
			}
			fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
				let (_, v) = bool_exp::<Ident>(v)
					.map_err(|e| E::custom(format!("invalid integer {e:?}")))?;
				Ok(v)
			}
		}
		deserializer.deserialize_str(V(PhantomData::<Identifier>))
	}
}

impl<Identifier: Display> Serialize for BoolExp<Identifier> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		serializer.serialize_str(&self.to_string())
	}
}

pub fn bool_exp<Identifier: FromStr>(input: &str) -> IResult<&str, BoolExp<Identifier>> {
	alt((
		map(verify(digit1, |s| matches!(s, "0" | "1")), |s| {
			BoolExp::Const(s == "1")
		}),
		one_arg,
		one_arg_set,
		two_arg,
		two_arg_int,
		two_arg_int_set,
		two_arg_set,
		two_arg_exp,
		var_arg,
		var_arg_exp,
		map(variable, BoolExp::Var),
	))(input)
}

fn one_arg<Identifier: FromStr>(input: &str) -> IResult<&str, BoolExp<Identifier>> {
	let (input, tag) = tag("not")(input)?;
	let (input, _) = char('(')(input)?;
	let (input, e) = bool_exp(input)?;
	let (input, _) = char(')')(input)?;
	Ok((
		input,
		match tag {
			"not" => BoolExp::Not,
			_ => unreachable!(),
		}(Box::new(e)),
	))
}

fn one_arg_set<Identifier: FromStr>(input: &str) -> IResult<&str, BoolExp<Identifier>> {
	let (input, tag) = tag("convex")(input)?;
	let (input, _) = char('(')(input)?;
	let (input, e) = set_exp(input)?;
	let (input, _) = char(')')(input)?;
	Ok((
		input,
		match tag {
			"convex" => BoolExp::Convex,
			_ => unreachable!(),
		}(Box::new(e)),
	))
}

fn two_arg<Identifier: FromStr>(input: &str) -> IResult<&str, BoolExp<Identifier>> {
	let (input, tag) = tag("imp")(input)?;
	let (input, _) = char('(')(input)?;
	let (input, e1) = bool_exp(input)?;
	let (input, _) = char(',')(input)?;
	let (input, e2) = bool_exp(input)?;
	let (input, _) = char(')')(input)?;
	Ok((
		input,
		match tag {
			"imp" => BoolExp::Implies,
			_ => unreachable!(),
		}(Box::new(e1), Box::new(e2)),
	))
}

fn two_arg_int<Identifier: FromStr>(input: &str) -> IResult<&str, BoolExp<Identifier>> {
	let (input, tag) = alt((tag("lt"), tag("le"), tag("gt"), tag("ge")))(input)?;
	let (input, _) = char('(')(input)?;
	let (input, e1) = int_exp(input)?;
	let (input, _) = char(',')(input)?;
	let (input, e2) = int_exp(input)?;
	let (input, _) = char(')')(input)?;
	Ok((
		input,
		match tag {
			"lt" => BoolExp::LessThan,
			"le" => BoolExp::LessThanEq,
			"gt" => BoolExp::GreaterThan,
			"ge" => BoolExp::GreaterThanEq,
			_ => unreachable!(),
		}(Box::new(e1), Box::new(e2)),
	))
}

fn two_arg_int_set<Identifier: FromStr>(input: &str) -> IResult<&str, BoolExp<Identifier>> {
	let (input, tag) = tag("in")(input)?;
	let (input, _) = char('(')(input)?;
	let (input, e1) = int_exp(input)?;
	let (input, _) = char(',')(input)?;
	let (input, e2) = set_exp(input)?;
	let (input, _) = char(')')(input)?;
	Ok((
		input,
		match tag {
			"in" => BoolExp::Member,
			_ => unreachable!(),
		}(Box::new(e1), Box::new(e2)),
	))
}

fn two_arg_set<Identifier: FromStr>(input: &str) -> IResult<&str, BoolExp<Identifier>> {
	let (input, tag) = alt((
		tag("disjoint"),
		tag("subset"),
		tag("subseq"),
		tag("supseq"),
		tag("supset"),
	))(input)?;
	let (input, _) = char('(')(input)?;
	let (input, e1) = set_exp(input)?;
	let (input, _) = char(',')(input)?;
	let (input, e2) = set_exp(input)?;
	let (input, _) = char(')')(input)?;
	Ok((
		input,
		match tag {
			"disjoint" => BoolExp::Disjoint,
			"subset" => BoolExp::SubSet,
			"subseq" => BoolExp::SubSetEq,
			"supseq" => BoolExp::SuperSetEq,
			"supset" => BoolExp::SuperSet,
			_ => unreachable!(),
		}(Box::new(e1), Box::new(e2)),
	))
}

fn two_arg_exp<Identifier: FromStr>(input: &str) -> IResult<&str, BoolExp<Identifier>> {
	let (input, tag) = tag("ne")(input)?;
	let (input, _) = char('(')(input)?;
	let (input, e1) = exp(input)?;
	let (input, _) = char(',')(input)?;
	let (input, e2) = exp(input)?;
	let (input, _) = char(')')(input)?;
	Ok((
		input,
		match tag {
			"imp" => BoolExp::NotEqual,
			_ => unreachable!(),
		}(Box::new(e1), Box::new(e2)),
	))
}

fn var_arg<Identifier: FromStr>(input: &str) -> IResult<&str, BoolExp<Identifier>> {
	let (input, tag) = alt((tag("and"), tag("or"), tag("xor"), tag("iff")))(input)?;
	let (input, _) = char('(')(input)?;
	let (input, es) = separated_list0(char(','), bool_exp)(input)?;
	let (input, _) = char(')')(input)?;
	Ok((
		input,
		match tag {
			"and" => BoolExp::And,
			"or" => BoolExp::Or,
			"xor" => BoolExp::Xor,
			"iff" => BoolExp::Equiv,
			_ => unreachable!(),
		}(es),
	))
}

fn var_arg_exp<Identifier: FromStr>(input: &str) -> IResult<&str, BoolExp<Identifier>> {
	let (input, tag) = tag("eq")(input)?;
	let (input, _) = char('(')(input)?;
	let (input, es) = separated_list1(char(','), exp)(input)?;
	let (input, _) = char(')')(input)?;
	Ok((
		input,
		match tag {
			"eq" => BoolExp::Equal,
			_ => unreachable!(),
		}(es),
	))
}
