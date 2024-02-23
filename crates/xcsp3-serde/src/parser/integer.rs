use std::ops::RangeInclusive;

use nom::{
	bytes::complete::tag,
	character::complete::{char, digit1},
	combinator::{map_res, opt, recognize},
	IResult,
};

use crate::IntVal;

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
	Ok((input, if let Some(_) = neg { -i } else { i }))
}
