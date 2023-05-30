use bitflags::bitflags;
use nom::{
    bytes::streaming::{tag, take},
    combinator::{complete, map, map_res},
    multi::count,
    number::streaming::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use strum_macros::FromRepr;
use thiserror::Error;

use self::constant_pool::Utf8Index;

#[derive(Debug)]
pub struct ClassFile<'a> {
    pub source_version: SourceVersion,
    pub constant_pool: constant_pool::ConstantPool<'a>,
    pub access_flags: AccessFlags,
    pub this_class: constant_pool::ClassIndex,
    pub super_class: constant_pool::ClassIndex,
    pub interfaces: Vec<constant_pool::ClassIndex>,
    pub fields: Vec<Field<'a>>,
    pub methods: Vec<Method<'a>>,
    pub attributes: Attributes<'a>,
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

#[derive(Debug)]
pub struct Attribute<'a> {
    pub name: Utf8Index,
    pub info: &'a [u8],
}

#[derive(Debug)]
pub struct Attributes<'a> {
    pub attributes: Vec<Attribute<'a>>,
}

#[derive(Debug)]
pub struct Field<'a> {
    pub access_flags: AccessFlags,
    pub name: constant_pool::Utf8Index,
    pub descriptor: constant_pool::FieldDescriptorIndex,
    pub attributes: Attributes<'a>,
}

#[derive(Debug)]
pub struct Method<'a> {
    pub access_flags: AccessFlags,
    pub name: constant_pool::Utf8Index,
    pub descriptor: constant_pool::MethodDescriptorIndex,
    pub attributes: Attributes<'a>,
}

#[derive(Debug, Error)]
pub enum ClassFormatError {
    #[error("Invalid class magic bytes")]
    InvalidMagicBytes,

    #[error("Unknown major source version: {0}")]
    UnknownMajorVersion(u16),
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

fn access_flags(input: &[u8]) -> IResult<&[u8], AccessFlags> {
    map(be_u16, AccessFlags::from_bits_retain)(input)
}

fn attribute(input: &[u8]) -> IResult<&[u8], Attribute<'_>> {
    let (input, name) = be_u16(input)?;
    let (input, length) = be_u32(input)?;
    let (input, info) = take(length)(input)?;
    let attribute = Attribute { name, info };
    Ok((input, attribute))
}

fn attributes(input: &[u8]) -> IResult<&[u8], Attributes<'_>> {
    let (input, length) = be_u16(input)?;
    let (input, attributes) = count(attribute, length as usize)(input)?;
    Ok((input, Attributes { attributes }))
}

fn interfaces(input: &[u8]) -> IResult<&[u8], Vec<u16>> {
    let (input, interfaces_count) = be_u16(input)?;
    count(be_u16, interfaces_count as usize)(input)
}

fn fields(input: &[u8]) -> IResult<&[u8], Vec<Field<'_>>> {
    let (input, fields_count) = be_u16(input)?;
    count(
        map(
            tuple((access_flags, be_u16, be_u16, attributes)),
            |(access_flags, name, descriptor, attributes)| Field {
                access_flags,
                name,
                descriptor,
                attributes,
            },
        ),
        fields_count as usize,
    )(input)
}

fn methods(input: &[u8]) -> IResult<&[u8], Vec<Method<'_>>> {
    let (input, methods_count) = be_u16(input)?;
    count(
        map(
            tuple((access_flags, be_u16, be_u16, attributes)),
            |(access_flags, name, descriptor, attributes)| Method {
                access_flags,
                name,
                descriptor,
                attributes,
            },
        ),
        methods_count as usize,
    )(input)
}

pub fn parse_class_file(input: &[u8]) -> IResult<&[u8], ClassFile> {
    complete(map(
        tuple((
            magic_bytes,
            source_version,
            constant_pool::take_constant_pool,
            access_flags,
            be_u16,
            be_u16,
            interfaces,
            fields,
            methods,
            attributes,
        )),
        |(
            _,
            source_version,
            constant_pool,
            access_flags,
            this_class,
            super_class,
            interfaces,
            fields,
            methods,
            attributes,
        )| ClassFile {
            source_version,
            constant_pool,
            access_flags,
            this_class,
            super_class,
            interfaces,
            fields,
            methods,
            attributes,
        },
    ))(input)
}

/// Module for parsers for the constant pool
mod constant_pool {
    use nom::{
        bytes::streaming::take,
        combinator::{fail, map, map_res},
        multi,
        number::streaming::{be_f32, be_f64, be_i32, be_i64, be_u16, be_u8, u8},
        sequence::tuple,
        IResult,
    };
    use strum_macros::FromRepr;
    use thiserror::Error;

    pub type PoolIndex = u16;
    pub type ClassIndex = PoolIndex;
    pub type Utf8Index = PoolIndex;
    pub type NameAndTypeIndex = PoolIndex;
    pub type DescriptorIndex = PoolIndex;
    pub type FieldDescriptorIndex = PoolIndex;
    pub type MethodDescriptorIndex = PoolIndex;

    #[derive(Debug)]
    pub struct ConstantPool<'a> {
        table: Vec<ConstantItem<'a>>,
    }

    impl<'a> ConstantPool<'a> {
        pub fn get(&self, index: PoolIndex) -> Option<&ConstantItem<'a>> {
            debug_assert!(index > 0);
            self.table.get((index - 1) as usize)
        }
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

            // TODO: Proper error
            _ => fail(input),
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
    use super::{magic_bytes, parse_class_file, source_version, MajorVersion};

    #[test]
    fn test_take_magic_bytes() {
        let bytes = [0xCA, 0xFE, 0xBA, 0xBE];
        magic_bytes(&bytes).unwrap();
    }

    #[test]
    fn test_take_invalid_magic_bytes() {
        let bytes = [0, 0, 0xBA, 0];
        magic_bytes(&bytes).unwrap_err();
    }

    #[test]
    fn test_take_source_version() {
        // Minor: 1, Major: Java 8,
        let bytes = [0, 1, 0, 52];
        let (_, version) = source_version(&bytes).unwrap();
        assert_eq!(version.minor, 1);
        assert_eq!(version.major, MajorVersion::Java8);
    }

    #[test]
    fn test_take_unknown_source_version() {
        // Minor: 1, Major: Unknown,
        let bytes = [0, 1, 0, 255];

        source_version(&bytes).unwrap_err();
    }

    #[test]
    fn test_take_class_file() {
        let example = include_bytes!("../tests/Example.class");
        let (_, class_file) = parse_class_file(example).unwrap();

        dbg!(class_file);
    }
}
