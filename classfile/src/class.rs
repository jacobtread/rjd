use bitflags::bitflags;
use nom::{
    bytes::streaming::tag,
    combinator::{map, map_res},
    multi::length_count,
    number::streaming::be_u16,
    sequence::tuple,
    IResult,
};
use strum_macros::FromRepr;
use thiserror::Error;

use crate::{
    attributes::{attributes, Attribute},
    constant_pool::{self, constant_pool, ConstantPool},
    types::{field_descriptor, method_descriptor, Class, FieldDesc, MethodDescriptor},
};

#[derive(Debug)]
pub struct ClassFile<'a> {
    pub source_version: SourceVersion,
    pub constant_pool: ConstantPool<'a>,
    pub access_flags: AccessFlags,
    pub this_class: Class<'a>,
    pub super_class: Option<Class<'a>>,
    pub interfaces: Vec<Class<'a>>,
    pub fields: Vec<Field<'a>>,
    pub methods: Vec<Method<'a>>,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SourceVersion {
    pub major: MajorVersion,
    pub minor: u16,
}

#[derive(Debug, PartialEq, Eq, FromRepr)]
#[repr(u16)]
pub enum MajorVersion {
    JavaLE4 = 48,
    Java5 = 49,
    Java6 = 50,
    Java7 = 51,
    Java8 = 52,
    Java9 = 53,
    Java10 = 54,
    Java11 = 55,
    Java12 = 56,
    Java13 = 57,
    Java14 = 58,
    Java15 = 59,
    Java16 = 60,
    Java17 = 61,
}

bitflags! {
    #[derive(Debug)]
    pub struct AccessFlags: u16 {
        const PUBLIC       = 0x0001;
        const PRIVATE      = 0x0002;
        const PROTECTED    = 0x0004;
        const STATIC       = 0x0008;
        const FINAL        = 0x0010;
        const SUPER        = 0x0020;
        const INTERFACE    = 0x0200;
        const ABSTRACT     = 0x0400;
        const SYNTHETIC    = 0x1000;
        const ANNOTATION   = 0x2000;
        const ENUM         = 0x4000;

        // Field only
        const VOLATILE     = 0x0040;
        const TRANSIENT    = 0x0080;

        // Method only
        const SYNCHRONIZED = 0x0020;
        const BRIDGE       = 0x0040;
        const VARARGS      = 0x0080;
        const NATIVE       = 0x0100;
        const STRICT       = 0x0800;
    }
}

#[derive(Debug, Error)]
pub enum FieldError {
    #[error("Constant for name didn't exist")]
    MissingName,
    #[error("Constant for descriptor didn't exist")]
    MissingDescriptor,
    #[error("Field descriptor was invalid")]
    InvalidDescriptor,
}

#[derive(Debug)]
pub struct Field<'a> {
    pub access_flags: AccessFlags,
    pub name: &'a str,
    pub descriptor: FieldDesc<'a>,
    pub attributes: Vec<Attribute>,
}

impl<'a> Field<'a> {
    fn new(
        pool: &ConstantPool<'a>,
        access_flags: AccessFlags,
        name: constant_pool::Utf8Index,
        descriptor: constant_pool::FieldDescriptorIndex,
        attributes: Vec<Attribute>,
    ) -> Result<Field<'a>, FieldError> {
        let name = pool.get_utf8(name).ok_or(FieldError::MissingName)?;
        let descriptor = pool
            .get_utf8(descriptor)
            .ok_or(FieldError::MissingDescriptor)?;
        let (_, descriptor) =
            field_descriptor(descriptor).map_err(|_| FieldError::InvalidDescriptor)?;

        Ok(Field {
            access_flags,
            name,
            descriptor,
            attributes,
        })
    }
}

#[derive(Debug, Error)]
pub enum MethodError {
    #[error("Constant for name didn't exist")]
    MissingName,
    #[error("Constant for descriptor didn't exist")]
    MissingDescriptor,
    #[error("Method descriptor was invalid")]
    InvalidDescriptor,
}

#[derive(Debug)]
pub struct Method<'a> {
    pub access_flags: AccessFlags,
    pub name: &'a str,
    pub descriptor: MethodDescriptor<'a>,
    pub attributes: Vec<Attribute>,
}

impl<'a> Method<'a> {
    fn new(
        pool: &ConstantPool<'a>,
        access_flags: AccessFlags,
        name: constant_pool::Utf8Index,
        descriptor: constant_pool::MethodDescriptorIndex,
        attributes: Vec<Attribute>,
    ) -> Result<Method<'a>, MethodError> {
        let name = pool.get_utf8(name).ok_or(MethodError::MissingName)?;
        let descriptor = pool
            .get_utf8(descriptor)
            .ok_or(MethodError::MissingDescriptor)?;
        let (_, descriptor) =
            method_descriptor(descriptor).map_err(|_| MethodError::InvalidDescriptor)?;

        Ok(Method {
            access_flags,
            name,
            descriptor,
            attributes,
        })
    }
}

#[derive(Debug, Error)]
pub enum ClassFormatError {
    #[error("Invalid class magic bytes")]
    InvalidMagicBytes,

    #[error("Unknown major source version: {0}")]
    UnknownMajorVersion(u16),

    #[error("Class constant points to unknown index")]
    InvalidClassRef,
}

fn magic_bytes(input: &[u8]) -> IResult<&[u8], &[u8]> {
    tag([0xCA, 0xFE, 0xBA, 0xBE])(input)
}

fn source_version(input: &[u8]) -> IResult<&[u8], SourceVersion> {
    map_res(tuple((be_u16, be_u16)), |(minor, major)| {
        MajorVersion::from_repr(major)
            .ok_or(ClassFormatError::UnknownMajorVersion(major))
            .map(|major| SourceVersion { major, minor })
    })(input)
}

pub fn access_flags(input: &[u8]) -> IResult<&[u8], AccessFlags> {
    map(be_u16, AccessFlags::from_bits_retain)(input)
}

pub fn fields<'b, 'a: 'b>(
    pool: &'b ConstantPool<'a>,
    input: &'a [u8],
) -> IResult<&'a [u8], Vec<Field<'a>>> {
    length_count(
        be_u16,
        map_res(
            tuple((access_flags, be_u16, be_u16, attributes(pool))),
            |(access_flags, name, descriptor, attributes)| {
                Field::new(pool, access_flags, name, descriptor, attributes)
            },
        ),
    )(input)
}

pub fn methods<'b, 'a: 'b>(
    pool: &'b ConstantPool<'a>,
    input: &'a [u8],
) -> IResult<&'a [u8], Vec<Method<'a>>> {
    length_count(
        be_u16,
        map_res(
            tuple((access_flags, be_u16, be_u16, attributes(pool))),
            |(access_flags, name, descriptor, attributes)| {
                Method::new(pool, access_flags, name, descriptor, attributes)
            },
        ),
    )(input)
}

pub fn class<'b, 'a: 'b>(
    pool: &'b ConstantPool<'a>,
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], Class<'a>> + 'b {
    map_res(be_u16, |index| {
        pool.get_class_name(index)
            .and_then(Class::try_parse)
            .ok_or(ClassFormatError::InvalidClassRef)
    })
}

pub fn parse_class_file(input: &[u8]) -> IResult<&[u8], ClassFile> {
    let (input, _) = magic_bytes(input)?;
    let (input, source_version) = source_version(input)?;
    let (input, constant_pool) = constant_pool(input)?;
    let (input, access_flags) = access_flags(input)?;

    let mut class_fn = class(&constant_pool);

    let (input, this_class) = class_fn(input)?;
    let (input, super_class) = map_res(be_u16, |index| {
        if index == 0 {
            Ok(None)
        } else {
            constant_pool
                .get_class_name(index)
                .and_then(Class::try_parse)
                .map(Some)
                .ok_or(ClassFormatError::InvalidClassRef)
        }
    })(input)?;

    let (input, interfaces) = length_count(be_u16, class_fn)(input)?;

    let (input, fields) = fields(&constant_pool, input)?;
    let (input, methods) = methods(&constant_pool, input)?;
    let (input, attributes) = attributes(&constant_pool)(input)?;

    let class = ClassFile {
        source_version,
        constant_pool,
        access_flags,
        this_class,
        super_class,
        interfaces,
        fields,
        methods,
        attributes,
    };
    Ok((input, class))
}

#[cfg(test)]
mod test {

    use super::{magic_bytes, parse_class_file, source_version, MajorVersion};

    #[test]
    fn test_magic_bytes() {
        let bytes = [0xCA, 0xFE, 0xBA, 0xBE];
        magic_bytes(&bytes).unwrap();
    }

    #[test]
    fn test_invalid_magic_bytes() {
        let bytes = [0, 0, 0xBA, 0];
        magic_bytes(&bytes).unwrap_err();
    }

    #[test]
    fn test_source_version() {
        // Minor: 1, Major: Java 8,
        let bytes = [0, 1, 0, 52];
        let (_, version) = source_version(&bytes).unwrap();
        assert_eq!(version.minor, 1);
        assert_eq!(version.major, MajorVersion::Java8);
    }

    #[test]
    fn test_unknown_source_version() {
        // Minor: 1, Major: Unknown,
        let bytes = [0, 1, 0, 255];

        source_version(&bytes).unwrap_err();
    }

    #[test]
    fn test_class_file() {
        let example = include_bytes!("../tests/Example.class");
        let (_, class_file) = parse_class_file(example).unwrap();

        dbg!(class_file);
    }
}
