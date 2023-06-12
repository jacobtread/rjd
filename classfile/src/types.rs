use nom::{
    branch::alt,
    bytes::complete::take_until1,
    character::complete::char,
    combinator::{map, value},
    multi::{many0, many1_count},
    sequence::{delimited, tuple},
    IResult,
};
use std::fmt::Display;

/// Parsed class type includes the package parts,
/// class name and outer class names
#[derive(Debug)]
pub struct Class<'a> {
    pub class: &'a str,
    pub packages: Vec<&'a str>,
    pub outer_classes: Vec<&'a str>,
}

impl<'a> Class<'a> {
    pub fn try_parse(s: &'a str) -> Option<Class<'a>> {
        let mut packages: Vec<&str> = s.split('/').collect();
        let class = packages.pop()?;

        let mut outer_classes: Vec<&str> = class.split('$').collect();
        let class = outer_classes.pop()?;

        Some(Self {
            class,
            packages,
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FieldDesc<'a> {
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
        dim: u8,
        /// The actual array type value
        ty: Box<FieldDesc<'a>>,
    },
    Void,
}

impl Display for FieldDesc<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Byte => "byte",
            Self::Char => "char",
            Self::Double => "double",
            Self::Float => "float",
            Self::Int => "int",
            Self::Long => "long",
            Self::Object(value) => match Class::try_parse(value) {
                Some(value) => {
                    return f.write_str(value.class);
                }
                None => {
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
            Self::Void => "void",
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct MethodDescriptor<'a> {
    pub parameters: Vec<FieldDesc<'a>>,
    /// Method return type (None indicating the void return type)
    pub return_type: FieldDesc<'a>,
}

pub fn field_descriptor(input: &str) -> IResult<&str, FieldDesc> {
    alt((primitive, object, array))(input)
}

fn primitive(input: &str) -> IResult<&str, FieldDesc> {
    // TODO: Might be better to replace with anychar and map
    alt((
        value(FieldDesc::Byte, char('B')),
        value(FieldDesc::Char, char('C')),
        value(FieldDesc::Double, char('D')),
        value(FieldDesc::Float, char('F')),
        value(FieldDesc::Int, char('I')),
        value(FieldDesc::Long, char('J')),
        value(FieldDesc::Short, char('S')),
        value(FieldDesc::Boolean, char('Z')),
        value(FieldDesc::Void, char('V')),
    ))(input)
}

fn array(input: &str) -> IResult<&str, FieldDesc> {
    map(
        tuple((many1_count(char('[')), field_descriptor)),
        |(dim, ty)| FieldDesc::Array {
            dim: dim as u8,
            ty: Box::new(ty),
        },
    )(input)
}

fn object(input: &str) -> IResult<&str, FieldDesc> {
    map(
        delimited(char('L'), take_until1(";"), char(';')),
        FieldDesc::Object,
    )(input)
}

pub fn method_descriptor(input: &str) -> IResult<&str, MethodDescriptor> {
    map(
        tuple((
            // Function Body: Open -> Descriptor* -> Close
            delimited(char('('), many0(field_descriptor), char(')')),
            // Return type: Descriptor
            field_descriptor,
        )),
        |(parameters, return_type)| MethodDescriptor {
            parameters,
            return_type,
        },
    )(input)
}

#[cfg(test)]
mod test {

    use crate::types::{field_descriptor, method_descriptor, MethodDescriptor};

    use super::FieldDesc;

    #[test]
    fn test_field_desc_array() {
        let value = "[[Lme/jacobtread/System;";
        let (_, desc) = field_descriptor(value).unwrap();
        assert_eq!(
            desc,
            FieldDesc::Array {
                dim: 2,
                ty: Box::new(FieldDesc::Object("me/jacobtread/System"))
            }
        );
    }

    #[test]
    fn test_field_desc_object() {
        let value = "Lme/jacobtread/System;";
        let (_, desc) = field_descriptor(value).unwrap();
        assert_eq!(desc, FieldDesc::Object("me/jacobtread/System"));
    }

    #[test]
    fn test_method_descriptor() {
        let value = "(Lme/jacobtread/System;[II)V";
        let (_, desc) = method_descriptor(value).unwrap();

        assert_eq!(
            desc,
            MethodDescriptor {
                parameters: vec![
                    FieldDesc::Object("me/jacobtread/System"),
                    FieldDesc::Array {
                        dim: 1,
                        ty: Box::new(FieldDesc::Int)
                    },
                    FieldDesc::Int
                ],
                return_type: FieldDesc::Void,
            }
        );

        dbg!(desc);
    }
}
