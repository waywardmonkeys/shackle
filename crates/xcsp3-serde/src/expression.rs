use std::{fmt::Display, marker::PhantomData, ops::RangeInclusive, str::FromStr};

use nom::{
	branch::alt,
	bytes::complete::tag,
	character::complete::{alpha1, alphanumeric0, char, digit1, multispace0, multispace1},
	combinator::{all_consuming, map, map_res, opt, recognize, verify},
	multi::{many0, separated_list0, separated_list1},
	sequence::{delimited, pair, preceded, separated_pair, terminated},
	IResult,
};
use serde::{de::Visitor, Deserialize, Deserializer, Serialize, Serializer};

use crate::{IntVal, VarRef, RESERVED};

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

#[derive(Clone, Debug, PartialEq, Hash)]
pub enum Exp<Identifier> {
	Bool(Box<BoolExp<Identifier>>),
	Int(Box<IntExp<Identifier>>),
	Set(Box<SetExp<Identifier>>),
	Var(VarRef<Identifier>),
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

pub(crate) fn identifier<Identifier: FromStr>(input: &str) -> IResult<&str, Identifier> {
	let (input, v) = verify(
		recognize(nom::sequence::tuple((alpha1, alphanumeric0))),
		|s: &str| !RESERVED.contains(&s),
	)(input)?;
	Ok((
		input,
		match v.parse() {
			Ok(t) => t,
			Err(_) => panic!("unable to create identifier"),
		},
	))
}

pub(crate) fn int(input: &str) -> IResult<&str, IntVal> {
	let (input, neg) = opt(char('-'))(input)?;
	let (input, i): (_, i64) = map_res(recognize(digit1), str::parse)(input)?;
	Ok((input, if neg.is_some() { -i } else { i }))
}

pub(crate) fn range(input: &str) -> IResult<&str, RangeInclusive<IntVal>> {
	let (input, lb) = int(input)?;
	if let (input, Some(_)) = opt(tag(".."))(input)? {
		let (input, ub) = int(input)?;
		Ok((input, lb..=ub))
	} else {
		Ok((input, lb..=lb))
	}
}

/// Parser combinator that repeatedly calls a parser consuming whitespace in
/// between when possible
pub(crate) fn sequence<'a, O>(
	p: impl FnMut(&'a str) -> IResult<&'a str, O>,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>> {
	terminated(many0(preceded(multispace0, p)), multispace0)
}

/// Parser combinator that expects parentheses with a comma seperated parser
/// rules
pub(crate) fn tuple<'a, O>(
	p: impl FnMut(&'a str) -> IResult<&'a str, O>,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>> {
	delimited(char('('), separated_list1(char(','), p), char(')'))
}

/// Parser combinator that requires whitespace between parsing rules
pub(crate) fn whitespace_seperated<'a, O>(
	p: impl FnMut(&'a str) -> IResult<&'a str, O>,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<O>> {
	separated_list1(multispace1, p)
}

impl<Identifier: FromStr> BoolExp<Identifier> {
	fn call_arg1(input: &str) -> IResult<&str, Self> {
		let (input, tag) = tag("not")(input)?;
		let (input, _) = char('(')(input)?;
		let (input, e) = Self::parse(input)?;
		let (input, _) = char(')')(input)?;
		Ok((
			input,
			match tag {
				"not" => BoolExp::Not,
				_ => unreachable!(),
			}(Box::new(e)),
		))
	}

	fn call_arg1_set(input: &str) -> IResult<&str, Self> {
		let (input, tag) = tag("convex")(input)?;
		let (input, _) = char('(')(input)?;
		let (input, e) = SetExp::parse(input)?;
		let (input, _) = char(')')(input)?;
		Ok((
			input,
			match tag {
				"convex" => BoolExp::Convex,
				_ => unreachable!(),
			}(Box::new(e)),
		))
	}

	fn call_arg2(input: &str) -> IResult<&str, Self> {
		let (input, tag) = tag("imp")(input)?;
		let (input, _) = char('(')(input)?;
		let (input, e1) = Self::parse(input)?;
		let (input, _) = char(',')(input)?;
		let (input, e2) = Self::parse(input)?;
		let (input, _) = char(')')(input)?;
		Ok((
			input,
			match tag {
				"imp" => BoolExp::Implies,
				_ => unreachable!(),
			}(Box::new(e1), Box::new(e2)),
		))
	}

	fn call_arg2_exp(input: &str) -> IResult<&str, Self> {
		let (input, tag) = tag("ne")(input)?;
		let (input, _) = char('(')(input)?;
		let (input, e1) = Exp::parse(input)?;
		let (input, _) = char(',')(input)?;
		let (input, e2) = Exp::parse(input)?;
		let (input, _) = char(')')(input)?;
		Ok((
			input,
			match tag {
				"imp" => BoolExp::NotEqual,
				_ => unreachable!(),
			}(Box::new(e1), Box::new(e2)),
		))
	}

	fn call_arg2_int(input: &str) -> IResult<&str, Self> {
		let (input, tag) = alt((tag("lt"), tag("le"), tag("gt"), tag("ge")))(input)?;
		let (input, _) = char('(')(input)?;
		let (input, e1) = IntExp::parse(input)?;
		let (input, _) = char(',')(input)?;
		let (input, e2) = IntExp::parse(input)?;
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

	fn call_arg2_int_set(input: &str) -> IResult<&str, Self> {
		let (input, tag) = tag("in")(input)?;
		let (input, _) = char('(')(input)?;
		let (input, e1) = IntExp::parse(input)?;
		let (input, _) = char(',')(input)?;
		let (input, e2) = SetExp::parse(input)?;
		let (input, _) = char(')')(input)?;
		Ok((
			input,
			match tag {
				"in" => BoolExp::Member,
				_ => unreachable!(),
			}(Box::new(e1), Box::new(e2)),
		))
	}

	fn call_arg2_set(input: &str) -> IResult<&str, Self> {
		let (input, tag) = alt((
			tag("disjoint"),
			tag("subset"),
			tag("subseq"),
			tag("supseq"),
			tag("supset"),
		))(input)?;
		let (input, _) = char('(')(input)?;
		let (input, e1) = SetExp::parse(input)?;
		let (input, _) = char(',')(input)?;
		let (input, e2) = SetExp::parse(input)?;
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

	fn call_argn(input: &str) -> IResult<&str, Self> {
		let (input, tag) = alt((tag("and"), tag("or"), tag("xor"), tag("iff")))(input)?;
		let (input, _) = char('(')(input)?;
		let (input, es) = separated_list0(char(','), Self::parse)(input)?;
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

	fn call_argn_exp(input: &str) -> IResult<&str, Self> {
		let (input, tag) = tag("eq")(input)?;
		let (input, _) = char('(')(input)?;
		let (input, es) = separated_list1(char(','), Exp::parse)(input)?;
		let (input, _) = char(')')(input)?;
		Ok((
			input,
			match tag {
				"eq" => BoolExp::Equal,
				_ => unreachable!(),
			}(es),
		))
	}

	pub(crate) fn parse(input: &str) -> IResult<&str, Self> {
		alt((
			map(verify(digit1, |s| matches!(s, "0" | "1")), |s| {
				BoolExp::Const(s == "1")
			}),
			Self::call_arg1,
			Self::call_arg1_set,
			Self::call_arg2,
			Self::call_arg2_int,
			Self::call_arg2_int_set,
			Self::call_arg2_set,
			Self::call_arg2_exp,
			Self::call_argn,
			Self::call_argn_exp,
			map(VarRef::parse, BoolExp::Var),
		))(input)
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
				let (_, v) = Self::Value::parse(v)
					.map_err(|e| E::custom(format!("invalid integer {e:?}")))?;
				Ok(v)
			}
		}
		deserializer.deserialize_str(V(PhantomData::<Identifier>))
	}
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

impl<Identifier: Display> Serialize for BoolExp<Identifier> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		serializer.serialize_str(&self.to_string())
	}
}

impl<Identifier: FromStr> Exp<Identifier> {
	pub(crate) fn parse(input: &str) -> IResult<&str, Self> {
		alt((
			map(VarRef::parse, |x| Exp::Var(x)),
			map(SetExp::parse, |x| Exp::Set(Box::new(x))),
			map(BoolExp::parse, |x| Exp::Bool(Box::new(x))),
			map(IntExp::parse, |x| Exp::Int(Box::new(x))),
		))(input)
	}

	pub(crate) fn parse_vec<'de, D: Deserializer<'de>>(
		deserializer: D,
	) -> Result<Vec<Self>, D::Error> {
		struct V<X>(PhantomData<X>);
		impl<'de, X: FromStr> Visitor<'de> for V<X> {
			type Value = Vec<Exp<X>>;

			fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
				formatter.write_str("a list of expressions")
			}

			fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
				let (_, v) = all_consuming(whitespace_seperated(Exp::parse))(v)
					.map_err(|e| E::custom(format!("invalid expressions {e:?}")))?;
				Ok(v)
			}
		}
		let visitor = V::<Identifier>(PhantomData);
		deserializer.deserialize_str(visitor)
	}
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

impl<Identifier: FromStr> IntExp<Identifier> {
	fn call_arg1(input: &str) -> IResult<&str, Self> {
		let (input, tag) = alt((tag("neg"), tag("abs"), tag("sqrt"), tag("sqr")))(input)?;
		let (input, _) = char('(')(input)?;
		let (input, e) = Self::parse(input)?;
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

	fn call_arg1_set(input: &str) -> IResult<&str, Self> {
		let (input, tag) = tag("card")(input)?;
		let (input, _) = char('(')(input)?;
		let (input, e) = SetExp::parse(input)?;
		let (input, _) = char(')')(input)?;
		Ok((
			input,
			match tag {
				"card" => IntExp::Card,
				_ => unreachable!(),
			}(Box::new(e)),
		))
	}

	fn call_arg2(input: &str) -> IResult<&str, Self> {
		let (input, tag) =
			alt((tag("sub"), tag("div"), tag("mod"), tag("pow"), tag("dist")))(input)?;
		let (input, _) = char('(')(input)?;
		let (input, e1) = Self::parse(input)?;
		let (input, _) = char(',')(input)?;
		let (input, e2) = Self::parse(input)?;
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

	fn call_arg3(input: &str) -> IResult<&str, Self> {
		let (input, tag) = alt((tag("if"),))(input)?;
		let (input, _) = char('(')(input)?;
		let (input, e1) = BoolExp::parse(input)?;
		let (input, _) = char(',')(input)?;
		let (input, e2) = Self::parse(input)?;
		let (input, _) = char(',')(input)?;
		let (input, e3) = Self::parse(input)?;
		let (input, _) = char(')')(input)?;
		Ok((
			input,
			match tag {
				"if" => IntExp::If,
				_ => unreachable!(),
			}(e1, Box::new(e2), Box::new(e3)),
		))
	}

	fn call_argn(input: &str) -> IResult<&str, Self> {
		let (input, tag) = alt((tag("add"), tag("mul")))(input)?;
		let (input, _) = char('(')(input)?;
		let (input, es) = separated_list1(char(','), Self::parse)(input)?;
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

	fn call_argn_exp(input: &str) -> IResult<&str, Self> {
		let (input, tag) = alt((tag("min"), tag("max")))(input)?;
		let (input, _) = char('(')(input)?;
		let (input, es) = separated_list1(char(','), Exp::parse)(input)?;
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

	pub(crate) fn parse(input: &str) -> IResult<&str, Self> {
		alt((
			map(int, IntExp::Const),
			IntExp::call_arg1,
			IntExp::call_arg1_set,
			IntExp::call_arg2,
			IntExp::call_arg3,
			IntExp::call_argn,
			IntExp::call_argn_exp,
			map(VarRef::parse, IntExp::Var),
			map(BoolExp::parse, IntExp::Bool),
		))(input)
	}

	pub(crate) fn parse_vec<'de, D: Deserializer<'de>>(
		deserializer: D,
	) -> Result<Vec<Self>, D::Error> {
		struct V<X>(PhantomData<X>);
		impl<'de, X: FromStr> Visitor<'de> for V<X> {
			type Value = Vec<IntExp<X>>;

			fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
				formatter.write_str("a list of integers expressions")
			}

			fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
				let (_, v) = all_consuming(whitespace_seperated(IntExp::parse))(v)
					.map_err(|e| E::custom(format!("invalid integer expressions {e:?}")))?;
				Ok(v)
			}
		}
		let visitor = V::<Identifier>(PhantomData);
		deserializer.deserialize_str(visitor)
	}
}

impl<'de, Identifier: FromStr> Deserialize<'de> for IntExp<Identifier> {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<IntExp<Identifier>, D::Error> {
		struct V<Ident>(PhantomData<Ident>);
		impl<'de, Ident: FromStr> Visitor<'de> for V<Ident> {
			type Value = IntExp<Ident>;

			fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
				formatter.write_str("an integer")
			}

			fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
				let (_, v) = Self::Value::parse(v)
					.map_err(|e| E::custom(format!("invalid integer {e:?}")))?;
				Ok(v)
			}
		}
		deserializer.deserialize_str(V(PhantomData::<Identifier>))
	}
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

impl<Identifier: Display> Serialize for IntExp<Identifier> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		serializer.serialize_str(&self.to_string())
	}
}

impl<Identifier: FromStr> SetExp<Identifier> {
	fn call_arg1(input: &str) -> IResult<&str, Self> {
		let (input, tag) = tag("hull")(input)?;
		let (input, _) = char('(')(input)?;
		let (input, e) = Self::parse(input)?;
		let (input, _) = char(')')(input)?;
		Ok((
			input,
			match tag {
				"hull" => SetExp::Hull,
				_ => unreachable!(),
			}(Box::new(e)),
		))
	}

	fn call_arg2(input: &str) -> IResult<&str, Self> {
		let (input, tag) = tag("diff")(input)?;
		let (input, _) = char('(')(input)?;
		let (input, e1) = Self::parse(input)?;
		let (input, _) = char(',')(input)?;
		let (input, e2) = Self::parse(input)?;
		let (input, _) = char(')')(input)?;
		Ok((
			input,
			match tag {
				"diff" => SetExp::Diff,
				_ => unreachable!(),
			}(Box::new(e1), Box::new(e2)),
		))
	}

	fn call_arg3(input: &str) -> IResult<&str, Self> {
		let (input, tag) = alt((tag("union"), tag("inter"), tag("sdiff")))(input)?;
		let (input, _) = char('(')(input)?;
		let (input, es) = separated_list1(char(','), Self::parse)(input)?;
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

	pub(crate) fn parse(input: &str) -> IResult<&str, Self> {
		alt((
			map(
				separated_pair(IntExp::parse, tag(".."), IntExp::parse),
				|(from, to)| SetExp::Range((from, to)),
			),
			Self::call_arg1,
			Self::call_arg2,
			Self::call_arg3,
			map(
				delimited(
					pair(tag("set"), char('(')),
					separated_list0(char(','), IntExp::parse),
					char(')'),
				),
				SetExp::Set,
			),
			map(VarRef::parse, SetExp::Var),
		))(input)
	}
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
