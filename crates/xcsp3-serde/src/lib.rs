pub(crate) mod constraint;
pub(crate) mod parser;
pub(crate) mod variable;

use std::{fmt::Display, str::FromStr};

use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::{constraint::Constraint, variable::Variable};

type IntVal = i64;

#[derive(Clone, PartialEq, Debug)]
pub struct Instance<Identifier = String> {
	ty: FrameworkType,
	variables: Vec<Variable<Identifier>>,
	constraints: Vec<Constraint<Identifier>>,
}

impl<'de, Identifier: Deserialize<'de> + FromStr> Deserialize<'de> for Instance<Identifier> {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		#[derive(Deserialize)]
		struct Variables<Identifier = String> {
			#[serde(rename = "$value")]
			content: Vec<Variable<Identifier>>,
		}
		#[derive(Deserialize)]
		struct Constraints<Identifier: FromStr = String> {
			#[serde(rename = "$value")]
			content: Vec<Constraint<Identifier>>,
		}
		#[derive(Deserialize)]
		pub struct X<Identifier: FromStr = String> {
			#[serde(rename = "@type")]
			ty: FrameworkType,
			variables: Variables<Identifier>,
			constraints: Constraints<Identifier>,
		}
		let inst: X<Identifier> = Deserialize::deserialize(deserializer)?;
		Ok(Self {
			ty: inst.ty,
			variables: inst.variables.content,
			constraints: inst.constraints.content,
		})
	}
}

impl<'de, Identifier: Serialize + Display> Serialize for Instance<Identifier> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		#[derive(Serialize)]
		struct Variables<'a, Identifier = String> {
			#[serde(rename = "$value")]
			content: Vec<V<'a, Identifier>>,
		}
		// Unit struct wrapper
		#[derive(Serialize)]
		enum V<'a, Identifier> {
			#[serde(rename = "var")]
			Var(&'a Variable<Identifier>),
		}
		#[derive(Serialize)]
		struct Constraints<'a, Identifier: Display = String> {
			#[serde(rename = "$value", default)]
			content: &'a Vec<Constraint<Identifier>>,
		}
		#[derive(Serialize)]
		pub struct X<'a, Identifier: Display = String> {
			#[serde(rename = "@type")]
			ty: FrameworkType,
			variables: Variables<'a, Identifier>,
			constraints: Constraints<'a, Identifier>,
		}
		let x = X {
			ty: self.ty,
			variables: Variables {
				content: self.variables.iter().map(V::Var).collect(),
			},
			constraints: Constraints {
				content: &self.constraints,
			},
		};
		Serialize::serialize(&x, serializer)
	}
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

#[cfg(test)]
mod tests {
	use std::{fs::File, io::BufReader, path::Path};

	use expect_test::ExpectFile;

	use crate::Instance;

	test_file!(xcsp3_core_ex_1);

	fn test_successful_serialization(file: &Path, exp: ExpectFile) {
		let rdr = BufReader::new(File::open(file).unwrap());
		let inst: Instance = quick_xml::de::from_reader(rdr).unwrap();
		exp.assert_debug_eq(&inst);
		let output = quick_xml::se::to_string(&inst).unwrap();
		let inst2: Instance = quick_xml::de::from_str(&output).unwrap();
		assert_eq!(inst, inst2)
	}

	macro_rules! test_file {
		($file: ident) => {
			#[test]
			fn $file() {
				test_successful_serialization(
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
}
