use nom::{
	character::complete::{char, multispace0, multispace1},
	multi::{many0, separated_list1},
	sequence::{delimited, preceded, terminated},
	IResult,
};

pub(crate) mod identifier;
pub(crate) mod integer;

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
