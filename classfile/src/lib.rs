use nom::{
    bytes::streaming::{tag, take},
    combinator::{map, map_res, verify},
    error::{self, Error, ErrorKind},
    number::streaming::be_u16,
    sequence::tuple,
    ErrorConvert, IResult,
};
use strum_macros::FromRepr;
use thiserror::Error;

#[derive(Debug)]
pub struct ClassFile<'a> {
    pub source_version: SourceVersion,
    pub constant_pool: constant_pool::ConstantPool<'a>,
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

#[derive(Debug, Error)]
pub enum ClassFormatError {
    #[error("Invalid class magic bytes")]
    InvalidMagicBytes,

    #[error("Unknown major source version: {0}")]
    UnknownMajorVersion(u16),
}

fn take_magic_bytes(input: &[u8]) -> IResult<&[u8], &[u8]> {
    tag([0xCA, 0xFE, 0xBA, 0xBE])(input)
}

fn take_source_version(input: &[u8]) -> IResult<&[u8], SourceVersion> {
    map_res(tuple((be_u16, be_u16)), |(minor, major)| {
        MajorVersion::from_repr(major)
            .ok_or(ClassFormatError::UnknownMajorVersion(major))
            .map(|major| SourceVersion { major, minor })
    })(input)
}

fn take_class_file(input: &[u8]) -> IResult<&[u8], ClassFile> {
    let (input, _) = take_magic_bytes(input)?;
    let (input, source_version) = take_source_version(input)?;
    let (input, constant_pool) = constant_pool::take_constant_pool(input)?;

    let class_file = ClassFile {
        source_version,
        constant_pool,
    };

    Ok((input, class_file))
}

/// Module for parsers for the constant pool
mod constant_pool {
    use nom::{
        bytes::streaming::take,
        combinator::{map, map_res},
        error::ErrorKind,
        multi,
        number::streaming::{be_f32, be_f64, be_i32, be_i64, be_u16, be_u8, u8},
        sequence::tuple,
        Err, IResult,
    };
    use strum_macros::FromRepr;
    use thiserror::Error;

    pub type PoolIndex = u16;
    pub type ClassIndex = u16;
    pub type Utf8Index = u16;
    pub type NameAndTypeIndex = u16;
    pub type DescriptorIndex = u16;
    pub type FieldDescriptorIndex = u16;
    pub type MethodDescriptorIndex = u16;

    #[derive(Debug)]
    pub struct ConstantPool<'a> {
        table: Vec<ConstantItem<'a>>,
    }

    #[derive(Debug)]
    pub enum ConstantItem<'a> {
        Utf8(&'a str),
        Integer(i32),
        Float(f32),
        Long(i64),
        Double(f64),
        Class(ClassInfo),
        Fieldref(ClassItem),
        Methodref(ClassItem),
        InterfaceMethodref(ClassItem),
        String(Utf8Index),
        NameAndType(NameAndType),
        MethodHandle(MethodHandle),
        MethodType(MethodType),
        InvokeDynamic(InvokeDynamic),
    }

    #[derive(Debug, Error)]
    pub enum ConstantError {
        #[error("Unknown constant type: {0}")]
        UnknownConstantType(u8),
        #[error("Unknown reference kind")]
        UnknownReferenceKind(u8),
    }

    pub fn take_constant_pool(input: &[u8]) -> IResult<&[u8], ConstantPool<'_>> {
        let (input, length) = be_u16(input)?;
        let length = (length - 1) as usize;
        let (input, table) = multi::count(take_constant_item, length)(input)?;
        Ok((input, ConstantPool { table }))
    }

    fn take_constant_item(input: &[u8]) -> IResult<&[u8], ConstantItem<'_>> {
        // Constants from https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4-140
        // Primitives
        const UTF8: u8 = 1;
        const INTEGER: u8 = 3;
        const FLOAT: u8 = 4;
        const LONG: u8 = 5;
        const DOUBLE: u8 = 6;

        // Lazy depending
        const CLASS: u8 = 7;
        const STRING: u8 = 8;

        const FIELDREF: u8 = 9;
        const METHODREF: u8 = 10;
        const INTERFACE_METHODREF: u8 = 11;

        const NAME_AND_TYPE: u8 = 12;
        const METHOD_HANDLE: u8 = 15;
        const METHOD_TYPE: u8 = 16;
        const INVOKE_DYNAMIC: u8 = 18;

        let (input, ty) = u8(input)?;

        match ty {
            UTF8 => take_utf8(input),

            INTEGER => map(be_i32, ConstantItem::Integer)(input),
            LONG => map(be_i64, ConstantItem::Long)(input),

            FLOAT => map(be_f32, ConstantItem::Float)(input),
            DOUBLE => map(be_f64, ConstantItem::Double)(input),

            CLASS => take_class_info(input),
            STRING => map(be_u16, ConstantItem::String)(input),

            FIELDREF => map(take_class_item, ConstantItem::Fieldref)(input),
            METHODREF => map(take_class_item, ConstantItem::Methodref)(input),
            INTERFACE_METHODREF => map(take_class_item, ConstantItem::InterfaceMethodref)(input),

            NAME_AND_TYPE => take_name_and_type(input),

            METHOD_HANDLE => take_method_handle(input),
            METHOD_TYPE => take_method_type(input),

            INVOKE_DYNAMIC => take_invoke_dynamic(input),

            _ => Err(Err::Error(nom::error::Error::new(input, ErrorKind::Alt))),
        }
    }

    fn take_utf8(input: &[u8]) -> IResult<&[u8], ConstantItem<'_>> {
        let (input, length) = be_u16(input)?;
        map_res(take(length), |bytes| {
            std::str::from_utf8(bytes).map(ConstantItem::Utf8)
        })(input)
    }

    fn take_class_info(input: &[u8]) -> IResult<&[u8], ConstantItem<'_>> {
        map(be_u16, |name| ConstantItem::Class(ClassInfo { name }))(input)
    }

    fn take_class_item(input: &[u8]) -> IResult<&[u8], ClassItem> {
        map(tuple((be_u16, be_u16)), |(class, name_and_type)| {
            ClassItem {
                class,
                name_and_type,
            }
        })(input)
    }

    fn take_name_and_type(input: &[u8]) -> IResult<&[u8], ConstantItem<'_>> {
        map(tuple((be_u16, be_u16)), |(name, descriptor)| {
            ConstantItem::NameAndType(NameAndType { name, descriptor })
        })(input)
    }

    fn take_method_type(input: &[u8]) -> IResult<&[u8], ConstantItem<'_>> {
        map(be_u16, |descriptor| {
            ConstantItem::MethodType(MethodType { descriptor })
        })(input)
    }

    fn take_method_handle(input: &[u8]) -> IResult<&[u8], ConstantItem<'_>> {
        map_res(
            tuple((be_u8, be_u16)),
            |(reference_kind, reference_index)| {
                ReferenceKind::from_repr(reference_kind)
                    .ok_or(ConstantError::UnknownReferenceKind)
                    .map(|reference_kind| {
                        ConstantItem::MethodHandle(MethodHandle {
                            reference_kind,
                            reference_index,
                        })
                    })
            },
        )(input)
    }

    fn take_invoke_dynamic(input: &[u8]) -> IResult<&[u8], ConstantItem<'_>> {
        map(
            tuple((be_u16, be_u16)),
            |(bootstrap_method_attr_index, name_and_type)| {
                ConstantItem::InvokeDynamic(InvokeDynamic {
                    bootstrap_method_attr_index,
                    name_and_type,
                })
            },
        )(input)
    }

    #[derive(Debug)]
    pub struct ClassInfo {
        pub name: Utf8Index,
    }

    /// Blanket type for Fieldref, Methodref, InterfaceMethodref
    #[derive(Debug)]
    pub struct ClassItem {
        pub class: ClassIndex,
        pub name_and_type: NameAndTypeIndex,
    }

    #[derive(Debug)]
    pub struct NameAndType {
        pub name: Utf8Index,
        pub descriptor: Utf8Index,
    }

    #[derive(Debug)]
    pub struct MethodHandle {
        pub reference_kind: ReferenceKind,
        pub reference_index: PoolIndex,
    }

    #[derive(Debug, FromRepr)]
    #[repr(u8)]
    pub enum ReferenceKind {
        GetField = 1,
        GetStatic = 2,
        PutField = 3,
        PutStatic = 4,
        InvokeVirtual = 5,
        InvokeStatic = 6,
        InvokeSpecial = 7,
        NewInvokeSpecial = 8,
        InvokeInterface = 9,
    }

    #[derive(Debug)]
    pub struct MethodType {
        pub descriptor: MethodDescriptorIndex,
    }

    #[derive(Debug)]
    pub struct InvokeDynamic {
        pub bootstrap_method_attr_index: u16,
        pub name_and_type: NameAndTypeIndex,
    }
}

#[cfg(test)]
mod test {
    use crate::{take_class_file, take_magic_bytes, take_source_version, MajorVersion};

    #[test]
    fn test_take_magic_bytes() {
        let bytes = [0xCA, 0xFE, 0xBA, 0xBE];
        take_magic_bytes(&bytes).unwrap();
    }

    #[test]
    fn test_take_invalid_magic_bytes() {
        let bytes = [0, 0, 0xBA, 0];
        take_magic_bytes(&bytes).unwrap_err();
    }

    #[test]
    fn test_take_source_version() {
        // Minor: 1, Major: Java 8,
        let bytes = [0, 1, 0, 52];
        let (_, version) = take_source_version(&bytes).unwrap();
        assert_eq!(version.minor, 1);
        assert_eq!(version.major, MajorVersion::Java8);
    }

    #[test]
    fn test_take_unknown_source_version() {
        // Minor: 1, Major: Unknown,
        let bytes = [0, 1, 0, 255];

        take_source_version(&bytes).unwrap_err();
    }

    #[test]
    fn test_take_class_file() {
        let example = include_bytes!("../tests/Example.class");
        let (_, class_file) = take_class_file(example).unwrap();

        dbg!(class_file);
    }
}
