use std::{fmt::Display, str::FromStr};

use serde::{Deserialize, Deserializer, Serialize};

use crate::{constraint::OffsetList, parser::integer::IntExp, MetaInfo};

#[derive(Clone, Debug, PartialEq, Hash, Serialize)]
#[serde(bound(serialize = "Identifier: Display"))]
pub struct Circuit<Identifier = String> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	pub list: OffsetList<Identifier>,
	#[serde(skip_serializing_if = "exp_is_zero")]
	pub size: IntExp<Identifier>,
}

impl<'de, Identifier: FromStr> Deserialize<'de> for Circuit<Identifier> {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		#[derive(Deserialize)]
		#[serde(bound = "Identifier: FromStr")]
		pub struct Circuit<Identifier> {
			#[serde(flatten)]
			info: MetaInfo<Identifier>,
			#[serde(default, alias = "$text")]
			simple: Vec<IntExp<Identifier>>,
			#[serde(default)]
			list: OffsetList<Identifier>,
			#[serde(default = "exp_zero", skip_serializing_if = "exp_is_zero")]
			size: IntExp<Identifier>,
		}
		let mut x = Circuit::deserialize(deserializer)?;
		if !x.simple.is_empty() {
			x.list = OffsetList {
				list: x.simple,
				start_index: 0,
			};
		}
		Ok(Self {
			info: x.info,
			list: x.list,
			size: x.size,
		})
	}
}

fn exp_zero<I>() -> IntExp<I> {
	IntExp::Const(0)
}
fn exp_is_zero<I>(val: &IntExp<I>) -> bool {
	matches!(val, IntExp::Const(0))
}
