pub mod constraint;
pub mod expression;

use std::{
	borrow::Cow,
	fmt::{self, Display},
	marker::PhantomData,
	ops::RangeInclusive,
	str::FromStr,
};

use flatzinc_serde::RangeList;
use nom::{
	character::complete::{char, digit1},
	combinator::{all_consuming, map_res, opt, recognize},
	multi::many0,
	sequence::delimited,
	IResult,
};
use serde::{de::Visitor, Deserialize, Deserializer, Serialize, Serializer};

use crate::{
	constraint::Constraint,
	expression::{identifier, int, range, sequence, whitespace_seperated, IntExp},
};

pub const RESERVED: &[&str] = &[
	"abs", "add", "and", "card", "convex", "diff", "disjoint", "dist", "div", "eq", "ge", "gt",
	"hull", "if", "iff", "imp", "in", "inter", "le", "lt", "max", "min", "mod", "mul", "ne", "neg",
	"not", "or", "pow", "sdiff", "set", "sqr", "sqrt", "sub", "subseq", "subset", "superseq",
	"superset", "union", "xor",
];

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Array<Identifier = String> {
	pub identifier: Identifier,
	pub note: Option<String>,
	pub size: Vec<usize>,
	pub domains: Vec<(Vec<VarRef<Identifier>>, RangeList<IntVal>)>,
}

#[derive(Clone, Debug, Default, PartialEq, Hash, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum CombinationType {
	#[default]
	Lexico,
	Pareto,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Deserialize, Serialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum FrameworkType {
	Csp,
	Cop,
	Wcsp,
	Fcsp,
	Qcsp,
	QcspPlus,
	Qcop,
	QcopPlus,
	Scsp,
	Scop,
	Qstr,
	Tcsp,
	Ncsp,
	Ncop,
	DisCsp,
	DisWcsp,
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum Index {
	/// Accessing a single index of a dimension in an array
	Single(IntVal),
	/// Accessing a slice of a dimension in an array
	Range(IntVal, IntVal),
	/// Accessing the full range of an array
	Full,
}

#[derive(Clone, PartialEq, Debug, Hash)]
pub struct Instance<Identifier = String> {
	/// The type of the framework used to express the instance.
	ty: FrameworkType,
	/// Definitions of the single decision variables
	variables: Vec<Variable<Identifier>>,
	/// Definitions of the arrays of decision variables
	arrays: Vec<Array<Identifier>>,
	/// Constraints that must be satisfied for a solution to be valid
	constraints: Vec<Constraint<Identifier>>,
	/// The objectives to be optimized
	objectives: Objectives<Identifier>,
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize)]
#[serde(bound(deserialize = "Identifier: FromStr"))]
pub struct Instantiation<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(rename = "@type", default, skip_serializing_if = "Option::is_none")]
	pub ty: Option<InstantiationType>,
	#[serde(rename = "@cost", default, skip_serializing_if = "Option::is_none")]
	pub cost: Option<IntVal>,
	#[serde(
		deserialize_with = "VarRef::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub list: Vec<VarRef<Identifier>>,
	#[serde(
		deserialize_with = "deserialize_int_vals",
		serialize_with = "serialize_list"
	)]
	pub values: Vec<IntVal>,
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum InstantiationType {
	Solution,
	Optimum,
}

pub type IntVal = i64;

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct MetaInfo<Identifier> {
	#[serde(
		rename = "@id",
		default,
		skip_serializing_if = "Option::is_none",
		deserialize_with = "deserialize_ident",
		serialize_with = "serialize_ident"
	)]
	pub identifier: Option<Identifier>,
	#[serde(rename = "@note", default, skip_serializing_if = "Option::is_none")]
	pub note: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(
	rename_all = "camelCase",
	bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display")
)]
pub enum Objective<Identifier = String> {
	#[serde(rename = "minimize")]
	Minimize(ObjExp<Identifier>),
	#[serde(rename = "maximize")]
	Maximize(ObjExp<Identifier>),
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Objectives<Identifier = String> {
	#[serde(default, rename = "@combination")]
	pub combination: CombinationType,
	#[serde(rename = "$value")]
	pub objectives: Vec<Objective<Identifier>>,
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct ObjExp<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(alias = "@type", default)]
	pub ty: ObjType,
	#[serde(
		alias = "$text",
		deserialize_with = "IntExp::parse_vec",
		serialize_with = "serialize_list"
	)]
	pub list: Vec<IntExp<Identifier>>,
	#[serde(
		default,
		skip_serializing_if = "Vec::is_empty",
		deserialize_with = "deserialize_int_vals",
		serialize_with = "serialize_list"
	)]
	pub coeffs: Vec<IntVal>,
}

#[derive(Clone, Debug, Default, PartialEq, Hash, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum ObjType {
	#[default]
	Sum,
	Minimum,
	Maximum,
	NValues,
	Lex,
}

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Variable<Identifier = String> {
	#[serde(
		rename = "@id",
		deserialize_with = "from_str",
		serialize_with = "as_str"
	)]
	pub identifier: Identifier,
	#[serde(rename = "@note", default, skip_serializing_if = "Option::is_none")]
	pub note: Option<String>,
	#[serde(
		rename = "$text",
		deserialize_with = "deserialize_range_list",
		serialize_with = "serialize_range_list"
	)]
	pub domain: RangeList<IntVal>,
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum VarRef<Identifier> {
	Ident(Identifier),
	ArrayAccess(Identifier, Vec<Index>),
}

fn as_str<S: Serializer, I: Display>(value: &I, serializer: S) -> Result<S::Ok, S::Error> {
	serializer.serialize_str(&value.to_string())
}

fn collect_range_list<I: IntoIterator<Item = RangeInclusive<IntVal>>>(
	iter: I,
) -> RangeList<IntVal> {
	let mut r: Vec<_> = iter.into_iter().collect();
	r.sort_by_key(|i| *i.start());
	let mut it = r.into_iter();
	let mut ranges = Vec::new();
	let mut cur = it.next().unwrap();
	for next in it {
		if *cur.end() >= (next.start() - 1) {
			cur = *cur.start()..=*next.end()
		} else {
			ranges.push(cur);
			cur = next;
		}
	}
	ranges.push(cur);
	ranges.into_iter().collect()
}

fn deserialize_ident<'de, D: Deserializer<'de>, Identifier: FromStr>(
	deserializer: D,
) -> Result<Option<Identifier>, D::Error> {
	struct V<X>(PhantomData<X>);
	impl<'de, X: FromStr> Visitor<'de> for V<X> {
		type Value = Option<X>;

		fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
			formatter.write_str("an identfier")
		}

		fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
			Ok(Some(FromStr::from_str(v).map_err(|_| {
				E::custom("unable to create identifier from string")
			})?))
		}
	}
	let visitor = V::<Identifier>(PhantomData);
	deserializer.deserialize_str(visitor)
}

fn deserialize_int_vals<'de, D: Deserializer<'de>>(
	deserializer: D,
) -> Result<Vec<IntVal>, D::Error> {
	struct V;
	impl<'de> Visitor<'de> for V {
		type Value = Vec<IntVal>;

		fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
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

fn deserialize_range_list<'de, D: Deserializer<'de>>(
	deserializer: D,
) -> Result<RangeList<IntVal>, D::Error> {
	struct V;
	impl<'de> Visitor<'de> for V {
		type Value = RangeList<IntVal>;

		fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
			formatter.write_str("a list of ranges")
		}

		fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
			let (_, r) = all_consuming(whitespace_seperated(range))(v)
				.map_err(|e| E::custom(format!("invalid list of ranges {e:?}")))?;
			Ok(collect_range_list(r))
		}
	}
	let visitor = V;
	deserializer.deserialize_str(visitor)
}

fn deserialize_size<'de, D: Deserializer<'de>>(deserializer: D) -> Result<Vec<usize>, D::Error> {
	struct V;
	impl<'de> Visitor<'de> for V {
		type Value = Vec<usize>;

		fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
			formatter.write_str("an array size expression")
		}

		fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
			let (_, r) = all_consuming(sequence(delimited(
				char('['),
				map_res(recognize(digit1), str::parse),
				char(']'),
			)))(v)
			.map_err(|e| E::custom(format!("invalid array size expression {e:?}")))?;
			Ok(r)
		}
	}
	let visitor = V;
	deserializer.deserialize_str(visitor)
}

fn from_str<'de, D: Deserializer<'de>, I: FromStr>(deserializer: D) -> Result<I, D::Error> {
	let s: Cow<'_, str> = Deserialize::deserialize(deserializer)?;
	match s.parse() {
		Ok(t) => Ok(t),
		Err(_) => Err(serde::de::Error::custom("unable to parse from string")),
	}
}

fn serialize_list<S: Serializer, T: Display>(exps: &[T], serializer: S) -> Result<S::Ok, S::Error> {
	serializer.serialize_str(
		&exps
			.iter()
			.map(|e| format!("{}", e))
			.collect::<Vec<_>>()
			.join(" "),
	)
}

fn serialize_ident<S: Serializer, Identifier: Display>(
	identifier: &Option<Identifier>,
	serializer: S,
) -> Result<S::Ok, S::Error> {
	serializer.serialize_str(&format!("{}", identifier.as_ref().unwrap()))
}

fn serialize_range_list<S: Serializer>(
	exps: &RangeList<IntVal>,
	serializer: S,
) -> Result<S::Ok, S::Error> {
	serializer.serialize_str(
		&exps
			.into_iter()
			.map(|e| {
				if e.start() == e.end() {
					e.start().to_string()
				} else {
					format!("{}..{}", e.start(), e.end())
				}
			})
			.collect::<Vec<_>>()
			.join(" "),
	)
}

fn serialize_size<S: Serializer>(exps: &[usize], serializer: S) -> Result<S::Ok, S::Error> {
	serializer.serialize_str(
		&exps
			.iter()
			.map(|e| format!("[{}]", e))
			.collect::<Vec<_>>()
			.join(""),
	)
}

impl<'de, Identifier: FromStr> Deserialize<'de> for Array<Identifier> {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		#[derive(Deserialize)]
		#[serde(bound = "Identifier: FromStr")]
		struct DomainStruct<Identifier: FromStr> {
			#[serde(rename = "@for", deserialize_with = "VarRef::parse_vec")]
			vars: Vec<VarRef<Identifier>>,
			#[serde(rename = "$text", deserialize_with = "deserialize_range_list")]
			domain: RangeList<IntVal>,
		}
		#[derive(Deserialize)]
		#[serde(bound = "Identifier: FromStr")]
		enum Domain<'a, Identifier: FromStr> {
			#[serde(rename = "domain")]
			Domain(Vec<DomainStruct<Identifier>>),
			#[serde(rename = "$text")]
			Direct(Cow<'a, str>),
		}
		#[derive(Deserialize)]
		#[serde(bound = "Identifier: FromStr")]
		struct Array<'a, Identifier: FromStr> {
			#[serde(rename = "@id", deserialize_with = "from_str")]
			identifier: Identifier,
			#[serde(rename = "@note", default, skip_serializing_if = "Option::is_none")]
			note: Option<String>,
			#[serde(rename = "@size", deserialize_with = "deserialize_size")]
			size: Vec<usize>,
			#[serde(rename = "$value")]
			domain: Domain<'a, Identifier>,
		}
		let x = Array::deserialize(deserializer)?;
		let domains = match x.domain {
			Domain::Domain(v) => v.into_iter().map(|d| (d.vars, d.domain)).collect(),
			Domain::Direct(s) => {
				let s = all_consuming(whitespace_seperated(range))(s.as_ref())
					.map_err(serde::de::Error::custom)?;
				vec![(
					vec![VarRef::Ident(
						Identifier::from_str("others").unwrap_or_else(|_| {
							panic!("unable to create identifier from `\"others\"`")
						}),
					)],
					collect_range_list(s.1),
				)]
			}
		};
		Ok(Self {
			identifier: x.identifier,
			note: x.note,
			size: x.size,
			domains,
		})
	}
}

impl<Identifier: Display> Serialize for Array<Identifier> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		#[derive(Serialize)]
		#[serde(bound = "Identifier: Display")]
		struct DomainStruct<'a, Identifier: Display> {
			#[serde(rename = "@for", serialize_with = "serialize_list")]
			vars: &'a Vec<VarRef<Identifier>>,
			#[serde(rename = "$text", serialize_with = "serialize_range_list")]
			domain: &'a RangeList<IntVal>,
		}
		#[derive(Serialize)]
		#[serde(bound = "Identifier: Display")]
		enum Domain<'a, Identifier: Display> {
			#[serde(rename = "domain")]
			Domain(DomainStruct<'a, Identifier>),
		}
		#[derive(Serialize)]
		#[serde(bound = "Identifier: Display")]
		struct Array<'a, Identifier: Display> {
			#[serde(rename = "@id", serialize_with = "as_str")]
			identifier: &'a Identifier,
			#[serde(rename = "@note", default, skip_serializing_if = "Option::is_none")]
			note: &'a Option<String>,
			#[serde(rename = "@size", serialize_with = "serialize_size")]
			size: &'a Vec<usize>,
			#[serde(rename = "$value")]
			domain: Vec<Domain<'a, Identifier>>,
		}
		let domain = self
			.domains
			.iter()
			.map(|(v, d)| Domain::Domain(DomainStruct { vars: v, domain: d }))
			.collect();
		let x = Array {
			identifier: &self.identifier,
			note: &self.note,
			size: &self.size,
			domain,
		};
		x.serialize(serializer)
	}
}

impl<'de, Identifier: Deserialize<'de> + FromStr> Deserialize<'de> for Instance<Identifier> {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		#[derive(Deserialize)]
		enum V<Identifier: FromStr> {
			#[serde(rename = "var")]
			Variable(Variable<Identifier>),
			#[serde(rename = "array")]
			Array(Array<Identifier>),
		}
		#[derive(Deserialize)]
		struct Variables<Identifier: FromStr = String> {
			#[serde(rename = "$value")]
			vars: Vec<V<Identifier>>,
		}
		#[derive(Deserialize)]
		struct Constraints<Identifier: FromStr = String> {
			#[serde(rename = "$value")]
			content: Vec<Constraint<Identifier>>,
		}
		#[derive(Deserialize)]
		struct Instance<Identifier: FromStr = String> {
			#[serde(rename = "@type")]
			ty: FrameworkType,
			variables: Option<Variables<Identifier>>,
			constraints: Option<Constraints<Identifier>>,
			#[serde(default = "Objectives::default")]
			objectives: Objectives<Identifier>,
		}
		let inst: Instance<Identifier> = Deserialize::deserialize(deserializer)?;
		let mut variables = Vec::new();
		let mut arrays = Vec::new();
		for v in inst.variables.map(|v| v.vars).into_iter().flatten() {
			match v {
				V::Variable(var) => variables.push(var),
				V::Array(arr) => arrays.push(arr),
			}
		}
		Ok(Self {
			ty: inst.ty,
			variables,
			arrays,
			constraints: inst.constraints.map_or_else(Vec::new, |c| c.content),
			objectives: inst.objectives,
		})
	}
}

impl<Identifier: Serialize + Display> Serialize for Instance<Identifier> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		#[derive(Serialize)]
		struct Variables<'a, Identifier: Display> {
			var: &'a Vec<Variable<Identifier>>,
			array: &'a Vec<Array<Identifier>>,
		}
		impl<'a, Identifier: Display> Variables<'a, Identifier> {
			fn is_empty(&self) -> bool {
				self.var.is_empty() && self.array.is_empty()
			}
		}
		#[derive(Serialize)]
		struct Constraints<'a, Identifier: Display> {
			#[serde(rename = "$value")]
			content: &'a Vec<Constraint<Identifier>>,
		}
		impl<'a, Identifier: Display> Constraints<'a, Identifier> {
			fn is_empty(&self) -> bool {
				self.content.is_empty()
			}
		}
		#[derive(Serialize)]
		#[serde(rename = "instance")]
		struct Instance<'a, Identifier: Display> {
			#[serde(rename = "@type")]
			ty: FrameworkType,
			#[serde(skip_serializing_if = "Variables::is_empty")]
			variables: Variables<'a, Identifier>,
			#[serde(skip_serializing_if = "Constraints::is_empty")]
			constraints: Constraints<'a, Identifier>,
			#[serde(skip_serializing_if = "Objectives::is_empty")]
			objectives: &'a Objectives<Identifier>,
		}
		let x = Instance {
			ty: self.ty,
			variables: Variables {
				var: &self.variables,
				array: &self.arrays,
			},
			constraints: Constraints {
				content: &self.constraints,
			},
			objectives: &self.objectives,
		};
		Serialize::serialize(&x, serializer)
	}
}

// Note: flatten of MetaInfo does not seem to work here (https://github.com/tafia/quick-xml/issues/761)
impl<Identifier: Display> Serialize for Instantiation<Identifier> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		#[derive(Serialize)]
		#[serde(rename = "instantiation", bound(serialize = "Identifier: Display"))]
		struct Instantiation<'a, Identifier = String> {
			#[serde(
				rename = "@id",
				skip_serializing_if = "Option::is_none",
				serialize_with = "serialize_ident"
			)]
			identifier: &'a Option<Identifier>,
			#[serde(rename = "@note", skip_serializing_if = "Option::is_none")]
			note: &'a Option<String>,
			#[serde(rename = "@type", skip_serializing_if = "Option::is_none")]
			ty: &'a Option<InstantiationType>,
			#[serde(rename = "@cost", skip_serializing_if = "Option::is_none")]
			cost: &'a Option<IntVal>,
			#[serde(serialize_with = "serialize_list")]
			list: &'a Vec<VarRef<Identifier>>,
			#[serde(serialize_with = "serialize_list")]
			values: &'a Vec<IntVal>,
		}
		Instantiation {
			identifier: &self.info.identifier,
			note: &self.info.note,
			ty: &self.ty,
			cost: &self.cost,
			list: &self.list,
			values: &self.values,
		}
		.serialize(serializer)
	}
}

impl<Identifier> Objectives<Identifier> {
	pub fn is_empty(&self) -> bool {
		self.objectives.is_empty()
	}
}

impl<Identifier> Default for Objectives<Identifier> {
	fn default() -> Self {
		Self {
			combination: CombinationType::default(),
			objectives: Vec::new(),
		}
	}
}

impl<Identifier: FromStr> VarRef<Identifier> {
	pub(crate) fn parse(input: &str) -> IResult<&str, Self> {
		let (input, ident) = identifier(input)?;
		let (input, v) = many0(delimited(char('['), opt(range), char(']')))(input)?;
		Ok((
			input,
			if v.is_empty() {
				VarRef::Ident(ident)
			} else {
				let v = v
					.into_iter()
					.map(|r| {
						r.map(|r| {
							if r.start() == r.end() {
								Index::Single(*r.start())
							} else {
								Index::Range(*r.start(), *r.end())
							}
						})
						.unwrap_or(Index::Full)
					})
					.collect();
				VarRef::ArrayAccess(ident, v)
			},
		))
	}

	fn parse_vec<'de, D: Deserializer<'de>>(deserializer: D) -> Result<Vec<Self>, D::Error> {
		struct V<X>(PhantomData<X>);
		impl<'de, X: FromStr> Visitor<'de> for V<X> {
			type Value = Vec<VarRef<X>>;

			fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
				formatter.write_str("a list of variable references")
			}

			fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
				let (_, v) = all_consuming(whitespace_seperated(VarRef::parse))(v)
					.map_err(|e| E::custom(format!("invalid variable references {e:?}")))?;
				Ok(v)
			}
		}
		let visitor = V::<Identifier>(PhantomData);
		deserializer.deserialize_str(visitor)
	}
}

impl<Identifier: Display> Display for VarRef<Identifier> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			VarRef::Ident(ident) => ident.fmt(f),
			VarRef::ArrayAccess(ident, v) => {
				write!(
					f,
					"{}{}",
					ident,
					v.iter()
						.map(|i| format!(
							"[{}]",
							match i {
								Index::Single(v) => v.to_string(),
								Index::Range(a, b) => format!("{}..{}", a, b),
								Index::Full => String::new(),
							}
						))
						.collect::<Vec<_>>()
						.join("")
				)
			}
		}
	}
}

#[cfg(test)]
mod tests {
	use std::{fmt::Debug, fs::File, io::BufReader, path::Path};

	use expect_test::ExpectFile;
	use serde::{de::DeserializeOwned, Serialize};

	use crate::{Instance, Instantiation};

	fn test_successful_serialization<T: Debug + DeserializeOwned + Serialize + PartialEq>(
		file: &Path,
		exp: ExpectFile,
	) {
		let rdr = BufReader::new(File::open(file).unwrap());
		let inst: T = quick_xml::de::from_reader(rdr).unwrap();
		exp.assert_debug_eq(&inst);
		let output = quick_xml::se::to_string(&inst).unwrap();
		let inst2: T = quick_xml::de::from_str(&output).unwrap();
		assert_eq!(inst, inst2)
	}

	macro_rules! test_file {
		($file:ident) => {
			test_file!($file, Instance);
		};
		($file:ident, $t:ident) => {
			#[test]
			fn $file() {
				test_successful_serialization::<$t>(
					std::path::Path::new(&format!("./corpus/{}.xml", stringify!($file))),
					expect_test::expect_file![&format!(
						"../corpus/{}.debug.txt",
						stringify!($file)
					)],
				)
			}
		};
	}
	pub(crate) use test_file;

	test_file!(knapsack);

	test_file!(xcsp3_ex_001);
	test_file!(xcsp3_ex_002);
	// test_file!(xcsp3_ex_003);
	test_file!(xcsp3_ex_004);
	test_file!(xcsp3_ex_005);
	test_file!(xcsp3_ex_006);
	test_file!(xcsp3_ex_007);
	// test_file!(xcsp3_ex_008);
	// test_file!(xcsp3_ex_009);
	// test_file!(xcsp3_ex_010);
	// test_file!(xcsp3_ex_011);
	// test_file!(xcsp3_ex_012);
	// test_file!(xcsp3_ex_013);
	// test_file!(xcsp3_ex_014);
	// test_file!(xcsp3_ex_015);
	// test_file!(xcsp3_ex_016);
	// test_file!(xcsp3_ex_017);
	test_file!(xcsp3_ex_018);
	test_file!(xcsp3_ex_019);
	// test_file!(xcsp3_ex_020);
	test_file!(xcsp3_ex_021);
	test_file!(xcsp3_ex_022);
	test_file!(xcsp3_ex_023, Instantiation);
	test_file!(xcsp3_ex_024);
	test_file!(xcsp3_ex_025, Instantiation);
	// test_file!(xcsp3_ex_026, Instantiation);
	test_file!(xcsp3_ex_027, Instantiation);
	// test_file!(xcsp3_ex_028, Instantiation);
	test_file!(xcsp3_ex_029);
	test_file!(xcsp3_ex_030);
	test_file!(xcsp3_ex_031);
	test_file!(xcsp3_ex_032);
	test_file!(xcsp3_ex_033);
	test_file!(xcsp3_ex_034);
	test_file!(xcsp3_ex_035);
	test_file!(xcsp3_ex_036);
	test_file!(xcsp3_ex_037);
	// test_file!(xcsp3_ex_038);
	// test_file!(xcsp3_ex_039);
	// test_file!(xcsp3_ex_040);
	test_file!(xcsp3_ex_041);
	// test_file!(xcsp3_ex_042);
	test_file!(xcsp3_ex_043);
	test_file!(xcsp3_ex_044);
	test_file!(xcsp3_ex_045);
	test_file!(xcsp3_ex_046);
	test_file!(xcsp3_ex_047);
	// test_file!(xcsp3_ex_048);
	test_file!(xcsp3_ex_049);
	// test_file!(xcsp3_ex_050);
	test_file!(xcsp3_ex_051);
	test_file!(xcsp3_ex_052);
	test_file!(xcsp3_ex_053);
	// test_file!(xcsp3_ex_054);
	test_file!(xcsp3_ex_055);
	test_file!(xcsp3_ex_056);
	test_file!(xcsp3_ex_057);
	test_file!(xcsp3_ex_058);
	test_file!(xcsp3_ex_059);
	test_file!(xcsp3_ex_060);
	// test_file!(xcsp3_ex_061);
	// test_file!(xcsp3_ex_062);
	test_file!(xcsp3_ex_063);
	test_file!(xcsp3_ex_064);
	test_file!(xcsp3_ex_065);
	test_file!(xcsp3_ex_066);
	test_file!(xcsp3_ex_067);
	test_file!(xcsp3_ex_068);
	test_file!(xcsp3_ex_069);
	// test_file!(xcsp3_ex_070);
	// test_file!(xcsp3_ex_071);
	test_file!(xcsp3_ex_072);
	// test_file!(xcsp3_ex_073);
	test_file!(xcsp3_ex_074);
	test_file!(xcsp3_ex_075);
	test_file!(xcsp3_ex_076);
	test_file!(xcsp3_ex_077);
	test_file!(xcsp3_ex_078);
	// test_file!(xcsp3_ex_079);
	// test_file!(xcsp3_ex_080);
	// test_file!(xcsp3_ex_081);
	// test_file!(xcsp3_ex_082);
	// test_file!(xcsp3_ex_083);
	// test_file!(xcsp3_ex_084);
	test_file!(xcsp3_ex_085);
	test_file!(xcsp3_ex_086);
	// test_file!(xcsp3_ex_087);
	// test_file!(xcsp3_ex_088);
	test_file!(xcsp3_ex_089);
	// test_file!(xcsp3_ex_090);
	test_file!(xcsp3_ex_091);
	// test_file!(xcsp3_ex_092);
	// test_file!(xcsp3_ex_093);
	// test_file!(xcsp3_ex_094);
	// test_file!(xcsp3_ex_095);
	// test_file!(xcsp3_ex_096);
	test_file!(xcsp3_ex_097);
	// test_file!(xcsp3_ex_098);
	// test_file!(xcsp3_ex_099);
	test_file!(xcsp3_ex_100);
	test_file!(xcsp3_ex_101);
	// test_file!(xcsp3_ex_102);
	// test_file!(xcsp3_ex_103);
	// test_file!(xcsp3_ex_104);
	// test_file!(xcsp3_ex_105);
	// test_file!(xcsp3_ex_106);
	// test_file!(xcsp3_ex_107);
	// test_file!(xcsp3_ex_108);
	// test_file!(xcsp3_ex_109);
	// test_file!(xcsp3_ex_110);
	// test_file!(xcsp3_ex_111);
	// test_file!(xcsp3_ex_112);
	// test_file!(xcsp3_ex_113);
	// test_file!(xcsp3_ex_114);
	// test_file!(xcsp3_ex_115);
	// test_file!(xcsp3_ex_116);
	// test_file!(xcsp3_ex_117);
	// test_file!(xcsp3_ex_118);
	// test_file!(xcsp3_ex_119);
	// test_file!(xcsp3_ex_120);
	// test_file!(xcsp3_ex_121);
	// test_file!(xcsp3_ex_122);
	// test_file!(xcsp3_ex_123);
	// test_file!(xcsp3_ex_124);
	// test_file!(xcsp3_ex_125);
	// test_file!(xcsp3_ex_126);
	// test_file!(xcsp3_ex_127);
	// test_file!(xcsp3_ex_128);
	// test_file!(xcsp3_ex_129);
	// test_file!(xcsp3_ex_130);
	// test_file!(xcsp3_ex_131);
	// test_file!(xcsp3_ex_132);
	// test_file!(xcsp3_ex_133);
	// test_file!(xcsp3_ex_134);
	// test_file!(xcsp3_ex_135);
	// test_file!(xcsp3_ex_136);
	// test_file!(xcsp3_ex_137);
	// test_file!(xcsp3_ex_138);
	// test_file!(xcsp3_ex_139);
	// test_file!(xcsp3_ex_140);
	// test_file!(xcsp3_ex_141);
	// test_file!(xcsp3_ex_142);
	// test_file!(xcsp3_ex_143);
	// test_file!(xcsp3_ex_144);
	// test_file!(xcsp3_ex_145);
	// test_file!(xcsp3_ex_146);
	// test_file!(xcsp3_ex_147);
	// test_file!(xcsp3_ex_148);
	// test_file!(xcsp3_ex_149);
	// test_file!(xcsp3_ex_150);
	// test_file!(xcsp3_ex_151);
	// test_file!(xcsp3_ex_152);
	// test_file!(xcsp3_ex_153);
	// test_file!(xcsp3_ex_154);
	// test_file!(xcsp3_ex_155);
	// test_file!(xcsp3_ex_156);
	// test_file!(xcsp3_ex_157);
	// test_file!(xcsp3_ex_158);
	// test_file!(xcsp3_ex_159);
	// test_file!(xcsp3_ex_160);
	// test_file!(xcsp3_ex_161);
	// test_file!(xcsp3_ex_162);
	// test_file!(xcsp3_ex_163);
	// test_file!(xcsp3_ex_164);
	// test_file!(xcsp3_ex_165);
	// test_file!(xcsp3_ex_166);
	test_file!(xcsp3_ex_167);
	// test_file!(xcsp3_ex_168);
	// test_file!(xcsp3_ex_169);
	// test_file!(xcsp3_ex_170);
	// test_file!(xcsp3_ex_171);
	// test_file!(xcsp3_ex_172);
	// test_file!(xcsp3_ex_173);
	// test_file!(xcsp3_ex_174);
	// test_file!(xcsp3_ex_175);
	// test_file!(xcsp3_ex_176);
	// test_file!(xcsp3_ex_177);
	// test_file!(xcsp3_ex_178);
	// test_file!(xcsp3_ex_179);
	// test_file!(xcsp3_ex_180);
	// test_file!(xcsp3_ex_181);
	// test_file!(xcsp3_ex_182);
	// test_file!(xcsp3_ex_183);
	// test_file!(xcsp3_ex_184);
	// test_file!(xcsp3_ex_185);
	// test_file!(xcsp3_ex_186);
	// test_file!(xcsp3_ex_187);
	// test_file!(xcsp3_ex_188);
	// test_file!(xcsp3_ex_189);
	// test_file!(xcsp3_ex_190);
	// test_file!(xcsp3_ex_191);
}
