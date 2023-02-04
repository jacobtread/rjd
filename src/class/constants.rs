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
pub enum FieldType {
    Byte,
    Char,
    Double,
    Float,
    Int,
    Long,
    Object(String),
    Short,
    Boolean,
    Array {
        /// The number of dimensions of the array
        dim: u8,
        /// The actual array type value
        ty: Box<FieldType>,
    },
}

#[derive(Debug, Error)]
pub enum FieldTypeError {
    #[error("Field descriptor was empty")]
    Empty,

    #[error("Array type didn't specify actual type only depth")]
    InvalidArrayType,

    #[error("Array type dimensions was more than 255")]
    OversizedArray,

    #[error("Couldn't resolve deeply linked type")]
    RecusionLimit,

    #[error("Object name didn't have closing semi-colon")]
    ObjectNotClosed,

    #[error("Unknown descriptor type")]
    Unknown,
}

type FieldTypeResult = Result<FieldType, FieldTypeError>;

impl FromStr for FieldType {
    type Err = FieldTypeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut chars = s.chars();
        Self::from_chars(&mut chars)
    }
}

impl FieldType {
    fn from_chars(chars: &mut Chars<'_>) -> FieldTypeResult {
        let prefix = chars.next().ok_or(FieldTypeError::Empty)?;
        Self::from_prefixed(prefix, chars)
    }

    fn from_prefixed(prefix: char, value: &mut Chars<'_>) -> FieldTypeResult {
        Ok(match prefix {
            '[' => return Self::from_str_array(value),
            'L' => return Self::from_class_name(value),
            'B' => FieldType::Byte,
            'C' => FieldType::Char,
            'D' => FieldType::Double,
            'F' => FieldType::Float,
            'I' => FieldType::Int,
            'J' => FieldType::Long,
            'S' => FieldType::Short,
            'Z' => FieldType::Boolean,
            _ => return Err(FieldTypeError::Unknown),
        })
    }

    fn from_class_name(value: &mut Chars<'_>) -> FieldTypeResult {
        let mut output = String::new();
        while let Some(value) = value.next() {
            if value == ';' {
                return Ok(FieldType::Object(output));
            }
            output.push(value);
        }
        return Err(FieldTypeError::ObjectNotClosed);
    }

    fn from_str_array(value: &mut Chars<'_>) -> FieldTypeResult {
        let mut dim = 1;

        while let Some(ch) = value.next() {
            if ch == '[' {
                if dim == 255 {
                    return Err(FieldTypeError::OversizedArray);
                }

                dim += 1;
            } else {
                let value = Self::from_prefixed(ch, value)?;
                return Ok(FieldType::Array {
                    dim,
                    ty: Box::new(value),
                });
            }
        }

        Err(FieldTypeError::InvalidArrayType)
    }
}

impl Display for FieldType {
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
                let suffix = "[]".repeat((*dim) as usize);
                return write!(f, "{}{}", ty, suffix);
            }
        })
    }
}

#[derive(Debug, Error)]
pub enum MetDescError {
    #[error("Method descriptor was empty")]
    Empty,

    #[error("Method descriptor didn't open with (")]
    InvalidOpening,

    #[error("Method descriptor wasn't closed")]
    MissingClosing,

    #[error("Method descriptor was missing return type")]
    MissingReturn,

    #[error("{0}")]
    FieldType(#[from] FieldTypeError),
}

#[derive(Debug)]
pub struct MethodDescriptor {
    pub parameters: Vec<FieldType>,
    /// Method return type (None indicating the void return type)
    pub return_type: Option<FieldType>,
}

impl FromStr for MethodDescriptor {
    type Err = MetDescError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut chars = s.chars();
        Self::from_chars(&mut chars)
    }
}

impl MethodDescriptor {
    pub fn from_chars(value: &mut Chars<'_>) -> Result<MethodDescriptor, MetDescError> {
        {
            let open = value.next().ok_or(MetDescError::Empty)?;
            if open != '(' {
                return Err(MetDescError::InvalidOpening);
            }
        }

        let mut parameters = Vec::new();
        let mut closed = false;
        while let Some(char) = value.next() {
            if char == ')' {
                closed = true;
                break;
            }

            let value = FieldType::from_prefixed(char, value)?;
            parameters.push(value);
        }

        if !closed {
            return Err(MetDescError::MissingClosing);
        }

        let return_type = match value.next() {
            Some('V') => None,
            Some(char) => Some(FieldType::from_prefixed(char, value)?),
            None => return Err(MetDescError::MissingReturn),
        };

        Ok(MethodDescriptor {
            parameters,
            return_type,
        })
    }
}

#[cfg(test)]
mod test {
    use std::str::FromStr;

    use crate::class::constants::MethodDescriptor;

    use super::FieldType;

    #[test]
    fn test_array_parse() {
        let value = "[[Lme/jacobtread/System;";
        let desc = FieldType::from_str(value).unwrap();
        dbg!(desc);
    }

    #[test]
    fn test_met() {
        let value = "([[[Lme/jacobtread/System;)I";
        let desc = MethodDescriptor::from_str(value).unwrap();
        dbg!(desc);
    }
}
