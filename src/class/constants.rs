use std::{
    fmt::{Debug, Display},
    str::{Chars, FromStr},
};

use thiserror::Error;

use crate::format::class::{ConstantItem, ConstantPool, ConstantPoolResolve, RawClass};

#[derive(Debug, Clone)]
pub struct Class<'a> {
    /// Packages associated with the class
    pub packages: Vec<&'a str>,
    /// The class itself
    pub class: &'a str,
    /// Outer classes for the class
    pub outer_classes: Vec<&'a str>,
}

impl<'a> ConstantPoolResolve<'a> for Class<'a> {
    type Error = ClassResolveError;

    fn resolve<'b>(value: &ConstantItem<'a>, pool: &ConstantPool<'a>) -> Result<Self, Self::Error> {
        let index = match value {
            ConstantItem::Class(RawClass { name: name_index }) => *name_index,
            _ => return Err(ClassResolveError::NonClassType),
        };

        // Find the associated UTF-8
        let utf8 = match pool.get(index) {
            Some(ConstantItem::Utf8(value)) => *value,
            Some(_) => return Err(ClassResolveError::NonUtf8Type),
            None => return Err(ClassResolveError::MissingClassUtf8),
        };

        Self::from_str(utf8)
    }
}

#[derive(Debug, Error)]
pub enum ClassResolveError {
    #[error("Claass path missing class name")]
    MissingClassName,

    #[error("Attempt to resolve object path from non utf-8 index")]
    NonUtf8Type,

    #[error("Missing backing ut8 constant for class")]
    MissingClassUtf8,

    #[error("Constant at desired index was not a Class type")]
    NonClassType,
}

impl<'a> Class<'a> {
    fn from_str(s: &'a str) -> Result<Self, ClassResolveError> {
        let mut packages: Vec<&str> = s.split('/').collect();
        let class = packages.pop().ok_or(ClassResolveError::MissingClassName)?;

        let mut outer_classes: Vec<&str> = class.split('$').collect();

        let class = outer_classes
            .pop()
            .ok_or(ClassResolveError::MissingClassName)?;

        Ok(Self {
            packages,
            class,
            outer_classes,
        })
    }

    pub fn is_java_lang(&self) -> bool {
        if self.packages.len() != 2 {
            return false;
        }

        self.packages[0] == "java" && self.packages[1] == "lang"
    }

    pub fn format_package(&self) -> String {
        self.packages.join(".")
    }
}

#[derive(Debug, PartialEq, Eq)]
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

    #[error("Attempt to resolve object path from non utf-8 index")]
    NonUtf8Type,
}

type FieldTypeResult = Result<FieldType, FieldTypeError>;

impl FromStr for FieldType {
    type Err = FieldTypeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut chars = s.chars();
        Self::from_chars(&mut chars)
    }
}

#[derive(Debug, Error)]
pub struct NonUtf8Error;

impl Display for NonUtf8Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Expected utf8 value at constant pool index")
    }
}

impl<'a> ConstantPoolResolve<'a> for &'a str {
    type Error = NonUtf8Error;

    fn resolve<'b>(
        value: &ConstantItem<'a>,
        _pool: &'b ConstantPool<'a>,
    ) -> Result<Self, Self::Error> {
        match value {
            ConstantItem::Utf8(value) => Ok(*value),
            _ => Err(NonUtf8Error),
        }
    }
}

impl<'a> ConstantPoolResolve<'a> for FieldType {
    type Error = FieldTypeError;

    fn resolve<'b>(
        value: &ConstantItem<'a>,
        _pool: &'b ConstantPool<'a>,
    ) -> Result<Self, Self::Error> {
        match value {
            ConstantItem::Utf8(value) => Self::from_str(value),
            _ => Err(FieldTypeError::NonUtf8Type),
        }
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
        for value in value.by_ref() {
            if value == ';' {
                return Ok(FieldType::Object(output));
            }
            output.push(value);
        }

        Err(FieldTypeError::ObjectNotClosed)
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
            Self::Object(value) => match Class::from_str(value) {
                Ok(value) => {
                    return f.write_str(value.class);
                }
                Err(_) => {
                    let value = value.replace('/', ".");
                    return f.write_str(&value);
                }
            },
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

    #[error("Attempt to resolve object path from non utf-8 index")]
    NonUtf8Type,
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

impl<'a> ConstantPoolResolve<'a> for MethodDescriptor {
    type Error = MetDescError;

    fn resolve<'b>(
        value: &ConstantItem<'a>,
        _pool: &'b ConstantPool<'a>,
    ) -> Result<Self, Self::Error> {
        match value {
            ConstantItem::Utf8(value) => Self::from_str(value),
            _ => Err(MetDescError::NonUtf8Type),
        }
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
        assert_eq!(
            desc,
            FieldType::Array {
                dim: 2,
                ty: Box::new(FieldType::Object("me/jacobtread/System".to_string()))
            }
        );
    }

    #[test]
    fn test_met() {
        let value = "([[[Lme/jacobtread/System;)I";
        let desc = MethodDescriptor::from_str(value).unwrap();

        dbg!(desc);
    }
}
