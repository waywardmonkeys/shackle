use std::str::FromStr;

use nom::{
	character::complete::{alpha1, alphanumeric0, char, multispace0},
	combinator::{recognize, verify},
	sequence::{delimited, preceded, tuple},
	IResult,
};

use crate::{parser::integer::int, variable::VarRef};

const RESERVED: &[&str] = &[
	"abs", "add", "and", "card", "convex", "diff", "disjoint", "dist", "div", "eq", "ge", "gt",
	"hull", "if", "iff", "imp", "in", "inter", "le", "lt", "max", "min", "mod", "mul", "ne", "neg",
	"not", "or", "pow", "sdiff", "set", "sqr", "sqrt", "sub", "subseq", "subset", "superseq",
	"superset", "union", "xor",
];

pub fn identifier<Identifier: FromStr>(input: &str) -> IResult<&str, Identifier> {
	let (input, v) = verify(recognize(tuple((alpha1, alphanumeric0))), |s: &str| {
		!RESERVED.contains(&s)
	})(input)?;
	Ok((
		input,
		match v.parse() {
			Ok(t) => t,
			Err(_) => panic!("unable to create identifier"),
		},
	))
}

pub fn variable<Identifier: FromStr>(input: &str) -> IResult<&str, VarRef<Identifier>> {
	let (input, ident) = identifier(input)?;
	if let Ok((input2, i)) = preceded(multispace0, delimited(char('['), int, char(']')))(input) {
		Ok((input2, VarRef::ArrayAccess(ident, i)))
	} else {
		Ok((input, VarRef::Ident(ident)))
	}
}
