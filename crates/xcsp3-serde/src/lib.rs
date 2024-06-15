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
			variables: Option<Variables<Identifier>>,
			constraints: Option<Constraints<Identifier>>,
		}
		let inst: X<Identifier> = Deserialize::deserialize(deserializer)?;
		Ok(Self {
			ty: inst.ty,
			variables: inst.variables.map_or_else(Vec::new, |v| v.content),
			constraints: inst.constraints.map_or_else(Vec::new, |c| c.content),
		})
	}
}

impl<Identifier: Serialize + Display> Serialize for Instance<Identifier> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		#[derive(Serialize)]
		struct Variables<'a, Identifier = String> {
			#[serde(rename = "$value")]
			content: Vec<V<'a, Identifier>>,
		}
		impl<'a, Identifier> Variables<'a, Identifier> {
			fn is_empty(&self) -> bool {
				self.content.is_empty()
			}
		}
		// Unit struct wrapper
		#[derive(Serialize)]
		enum V<'a, Identifier> {
			#[serde(rename = "var")]
			Var(&'a Variable<Identifier>),
		}
		#[derive(Serialize)]
		struct Constraints<'a, Identifier: Display = String> {
			#[serde(rename = "$value")]
			content: &'a Vec<Constraint<Identifier>>,
		}
		impl<'a, Identifier: Display> Constraints<'a, Identifier> {
			fn is_empty(&self) -> bool {
				self.content.is_empty()
			}
		}
		#[derive(Serialize)]
		pub struct Instance<'a, Identifier: Display = String> {
			#[serde(rename = "@type")]
			ty: FrameworkType,
			#[serde(skip_serializing_if = "Variables::is_empty")]
			variables: Variables<'a, Identifier>,
			#[serde(skip_serializing_if = "Constraints::is_empty")]
			constraints: Constraints<'a, Identifier>,
		}
		let x = Instance {
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

	test_file!(knapsack);

	test_file!(xcsp3_ex_001);
	test_file!(xcsp3_ex_002);
	// test_file!(xcsp3_ex_003);
	// test_file!(xcsp3_ex_004);
	test_file!(xcsp3_ex_005);
	// test_file!(xcsp3_ex_006);
	// test_file!(xcsp3_ex_007);
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
	// test_file!(xcsp3_ex_018);
	// test_file!(xcsp3_ex_019);
	// test_file!(xcsp3_ex_020);
	// test_file!(xcsp3_ex_021);
	// test_file!(xcsp3_ex_022);
	// test_file!(xcsp3_ex_023);
	test_file!(xcsp3_ex_024);
	// test_file!(xcsp3_ex_025);
	// test_file!(xcsp3_ex_026);
	// test_file!(xcsp3_ex_027);
	// test_file!(xcsp3_ex_028);
	// test_file!(xcsp3_ex_029);
	// test_file!(xcsp3_ex_030);
	// test_file!(xcsp3_ex_031);
	// test_file!(xcsp3_ex_032);
	// test_file!(xcsp3_ex_033);
	// test_file!(xcsp3_ex_034);
	test_file!(xcsp3_ex_035);
	test_file!(xcsp3_ex_036);
	test_file!(xcsp3_ex_037);
	// test_file!(xcsp3_ex_038);
	// test_file!(xcsp3_ex_039);
	// test_file!(xcsp3_ex_040);
	// test_file!(xcsp3_ex_041);
	// test_file!(xcsp3_ex_042);
	// test_file!(xcsp3_ex_043);
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
	// test_file!(xcsp3_ex_167);
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
