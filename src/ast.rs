use std::fmt;

use serde::Deserialize;
use serde_json::error::Error as JsonError;

use crate::ident::Ident;

pub fn parse(exp: &str) -> Result<Rule, JsonError> {
    serde_json::from_str(&exp)
}

// AST
#[derive(Deserialize, Debug)]
pub enum Rule {
    Empty,
    LitBool{value: bool},
    LitNumber{value: i32},
    LitText{value: String},
    Not{not: Box<Rule>},
    If{condition: Box<Rule>, consequence: Box<Rule>},
    Chain{value: Vec<Rule>},
    VariableRef{value: Ident},
    RecordRef{record: Box<Rule>, identifier: Ident},
    Foreach{var: Ident, list: Box<Rule>, body: Box<Rule>},
}

// #[serde(tag = "name")]
// #[serde(rename_all = "camelCase")]

/*
#[derive(Deserialize, Debug)]
pub struct Literal<T> {
    value: T,
}

#[derive(Deserialize, Debug)]
pub struct Not {
    pub not: Expr,
}

#[derive(Deserialize, Debug)]
pub struct If {
    pub condition: Expr,
    pub consequence: Expr,
}

#[derive(Debug)]
pub struct Chain {
    pub chain: Vec<Expr>,
}

#[derive(Deserialize, Debug)]
pub struct VariableRef {
    pub identifier: Ident,
}

#[derive(Deserialize, Debug)]
pub struct RecordRef {
    pub identifier: Ident,
    pub record: Expr,
}

#[derive(Deserialize, Debug)]
pub struct Foreach {
    pub list: Expr,
    pub body: Expr,
    pub boundVar: String,
}
*/

#[derive(Deserialize)]
#[serde(field_identifier, rename_all = "lowercase")]
enum PivotField {
    Right,
    Left,
    Center,
}

/*
impl<'de> Deserialize<'de> for Chain {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct ChainVisitor;

        impl<'de> Visitor<'de> for ChainVisitor {
            type Value = Chain;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("chain with left, center and right fields")
            }

            fn visit_map<V>(self, mut map: V) -> Result<Chain, V::Error>
            where
                V: MapAccess<'de>,
            {
                let mut right = None;
                let mut left = None;
                let mut center = None;
                while let Some(key) = map.next_key()? {
                    match key {
                        PivotField::Left => {
                            if left.is_some() {
                                return Err(de::Error::duplicate_field("left"));
                            }
                            left = Some(map.next_value()?);
                        }
                        PivotField::Right => {
                            if right.is_some() {
                                return Err(de::Error::duplicate_field("right"));
                            }
                            right = Some(map.next_value()?);
                        }
                        PivotField::Center => {
                            if center.is_some() {
                                return Err(de::Error::duplicate_field("center"));
                            }
                            center = Some(map.next_value()?);
                        }
                    }
                }

                let mut left: Vec<Expr> = left.ok_or_else(|| de::Error::missing_field("left"))?;
                let mut right: Vec<Expr> =
                    right.ok_or_else(|| de::Error::missing_field("right"))?;
                let center: Expr = center.ok_or_else(|| de::Error::missing_field("center"))?;

                left.push(center);
                left.append(&mut right);
                Ok(Chain { chain: left })
            }
        }

        const FIELDS: &'static [&'static str] = &["center", "right", "left"];
        deserializer.deserialize_struct("Chain", FIELDS, ChainVisitor)
    }
}
*/
