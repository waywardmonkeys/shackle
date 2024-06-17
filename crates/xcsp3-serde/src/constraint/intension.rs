use std::{fmt::Display, str::FromStr};

use serde::{Deserialize, Serialize};

use crate::{parser::boolean::BoolExp, MetaInfo};

#[derive(Clone, Debug, PartialEq, Hash, Deserialize, Serialize)]
#[serde(bound(deserialize = "Identifier: FromStr", serialize = "Identifier: Display"))]
pub struct Intension<Identifier> {
	#[serde(flatten)]
	pub info: MetaInfo<Identifier>,
	#[serde(alias = "$text")]
	pub function: BoolExp<Identifier>,
}
