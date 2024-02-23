use std::str::FromStr;

use nom::{
	character::complete::{alpha1, alphanumeric0, char, multispace0},
	combinator::recognize,
	sequence::{delimited, preceded, tuple},
	IResult,
};

use crate::{parser::integer::int, variable::VarRef};

pub fn identifier<Identifier: FromStr>(input: &str) -> IResult<&str, Identifier> {
	let (input, v) = recognize(tuple((alpha1, alphanumeric0)))(input)?;
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
