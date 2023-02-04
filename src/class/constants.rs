use std::{
    fmt::{Display, Write},
    str::{Chars, FromStr},
};

use thiserror::Error;

use crate::format::class::RawConstantPool;

pub struct ConstantPool<'a> {
    values: Vec<ConstantPoolItem<'a>>,
}

impl<'a> ConstantPool<'a> {
    // pub fn from_raw(raw: RawConstantPool<'_>) -> Self {
    //     let raw_values = raw.0;
    //     let values = Vec::with_capacity(raw_values.len());

    //     for value in &Self {}
    // }
}

pub enum ConstantPoolItem<'a> {
    Class { name: &'a str },
}

#[derive(Debug)]
pub enum FieldType<'a> {
    Byte,
    Char,
    Double,
    Float,
    Int,
    Long,
    Object(&'a str),
    Short,
    Boolean,
    Array {
        /// The number of dimensions of the array
        dim: usize,
        /// The actual array type value
        ty: Box<FieldType<'a>>,
    },
}

#[derive(Debug, Error)]
pub enum FieldTypeError {
    #[error("Field descriptor was empty")]
    Empty,

    #[error("Array type didn't specify actual type only depth")]
    InvalidArrayType,

    #[error("Couldn't resolve deeply linked type")]
    RecusionLimit,

    #[error("Object name didn't have closing semi-colon")]
    ObjectNotClosed,

    #[error("Unknown descriptor type")]
    Unknown,
}

impl<'a> FieldType<'a> {
    pub fn from_str(value: &str) -> Result<FieldType, FieldTypeError> {
        if value.starts_with('[') {
            return Self::from_str_array(value);
        }

        if value.starts_with('L') {
            return Self::from_class_name(value);
        }

        Ok(match value {
            "B" => FieldType::Byte,
            "C" => FieldType::Char,
            "D" => FieldType::Double,
            "F" => FieldType::Float,
            "I" => FieldType::Int,
            "J" => FieldType::Long,
            "S" => FieldType::Short,
            "Z" => FieldType::Boolean,
            _ => return Err(FieldTypeError::Unknown),
        })
    }

    fn from_class_name(value: &str) -> Result<FieldType, FieldTypeError> {
        if value.len() < 3 {
            return Err(FieldTypeError::Empty);
        }
        if !value.ends_with(';') {
            return Err(FieldTypeError::ObjectNotClosed);
        }
        let value = &value[1..value.len() - 1];
        Ok(FieldType::Object(value))
    }

    fn from_str_array(value: &str) -> Result<FieldType, FieldTypeError> {
        let last = value.rfind('[').ok_or(FieldTypeError::InvalidArrayType)?; /* Find last index of array specifier */
        let dim = last + 1;
        let value = value
            .get(last + 1..)
            .ok_or(FieldTypeError::InvalidArrayType)?;
        let ty = Self::from_str(value)?;
        Ok(FieldType::Array {
            dim,
            ty: Box::new(ty),
        })
    }
}

impl Display for FieldType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Byte => "byte",
            Self::Char => "char",
            Self::Double => "double",
            Self::Float => "float",
            Self::Int => "int",
            Self::Long => "long",
            Self::Object(value) => {
                let value = value.replace("/", ".");
                return f.write_str(&value);
            }
            Self::Short => "short",
            Self::Boolean => "boolean",
            Self::Array { dim, ty } => {
                let suffix = "[]".repeat(*dim);
                return write!(f, "{}{}", ty, suffix);
            }
        })
    }
}

#[cfg(test)]
mod test {
    use super::FieldType;

    #[test]
    fn test_array_parse() {
        let value = "[[Lme/jacobtread/System;";
        let desc = FieldType::from_str(value).unwrap();
        dbg!(desc);
    }
}
