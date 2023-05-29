use super::{
    access::{ClassAccessFlags, FieldAccessFlags, MethodAccessFlags},
    reader::{ByteReadable, ByteReader, ReadError, ReadResult},
};
use std::{
    fmt::Display,
    str::{self},
};
use strum_macros::{Display, FromRepr};

#[cfg(test)]
mod test {
    use std::fs::read;

    use super::RawClassFile;

    #[test]
    fn test_parse_class() {
        let file = read("Test.class").unwrap();
        let class_file = RawClassFile::try_read(&file).unwrap();
        dbg!(class_file);
    }
}

#[derive(Debug)]
pub struct RawClassFile<'a> {
    pub version: SourceVersion,
    pub constant_pool: ConstantPool<'a>,
    pub access_flags: ClassAccessFlags,
    pub this_class: ConstantPoolIndex,
    pub super_class: ConstantPoolIndex,
    pub interfaces: Vec<ConstantPoolIndex>,
    pub fields: Vec<RawField<'a>>,
    pub methods: Vec<RawMethod<'a>>,
    pub attributes: Vec<RawAttribute<'a>>,
}

impl<'a> RawClassFile<'a> {
    pub const MAGIC: u32 = 0xCAFEBABE;

    pub fn try_read(data: &[u8]) -> ReadResult<RawClassFile<'_>> {
        let mut reader = ByteReader::new(data);
        let class_file = RawClassFile::read(&mut reader)?;
        Ok(class_file)
    }
}

impl<'a> ByteReadable<'a> for RawClassFile<'a> {
    fn read(r: &mut ByteReader<'a>) -> ReadResult<Self> {
        let magic = r.u4()?;
        if magic != RawClassFile::MAGIC {
            return Err(ReadError::InvalidMagic(magic, Self::MAGIC));
        }

        let version = SourceVersion::read(r)?;
        let constant_pool = ConstantPool::read(r)?;
        let access_flags = ClassAccessFlags::read(r)?;
        let this_class = ConstantPoolIndex::read(r)?;
        let super_class = ConstantPoolIndex::read(r)?;
        let interfaces = r.u2_list(false)?;
        let fields = r.u2_list(false)?;
        let methods = r.u2_list(false)?;
        let attributes = r.u2_list(false)?;

        Ok(Self {
            version,
            constant_pool,
            access_flags,
            this_class,
            super_class,
            interfaces,
            fields,
            methods,
            attributes,
        })
    }
}

#[derive(Debug)]
pub struct ConstantPool<'a> {
    pub values: Vec<ConstantItem<'a>>,
}

pub trait ConstantPoolResolve<'a>: Sized {
    type Error;

    fn resolve(value: &ConstantItem<'a>, pool: &ConstantPool<'a>) -> Result<Self, Self::Error>;
}

impl<'a> ConstantPool<'a> {
    pub fn get(&self, index: ConstantPoolIndex) -> Option<&ConstantItem<'a>> {
        self.values.get((index - 1) as usize)
    }

    pub fn resolve<'b, R, E>(&'b self, index: ConstantPoolIndex) -> Option<Result<R, E>>
    where
        R: ConstantPoolResolve<'a, Error = E>,
    {
        let value = self.values.get((index - 1) as usize)?;
        Some(R::resolve(value, self))
    }
}

impl<'a> ByteReadable<'a> for ConstantPool<'a> {
    fn read(r: &mut ByteReader<'a>) -> ReadResult<Self> {
        let values = r.u2_list(true)?;
        Ok(Self { values })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct SourceVersion {
    major: MajorVersion,
    minor: u16,
}

impl Display for SourceVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} (^{})", self.major, self.minor)
    }
}

impl ByteReadable<'_> for SourceVersion {
    fn read(r: &mut ByteReader<'_>) -> ReadResult<Self> {
        let minor = r.u2()?;
        let major = r.u2()?;

        match MajorVersion::from_repr(major) {
            Some(major) => Ok(Self { major, minor }),
            None => Err(ReadError::UnknownMajorVersion(major)),
        }
    }
}

#[derive(Debug, Display, PartialEq, Eq, FromRepr)]
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

pub type ConstantPoolIndex = u16;
pub type ClassIndex = u16;
pub type Utf8Index = u16;
pub type NameAndTypeIndex = u16;
pub type DescriptorIndex = u16;
pub type FieldDescriptorIndex = u16;
pub type MethodDescriptorIndex = u16;

/// Represents an item within the constant pool that is
/// not yet resolved
#[derive(Debug)]
pub enum ConstantItem<'a> {
    Utf8(&'a str),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    Class(RawClass),
    FieldRef(RawClassItem),
    MethodRef(RawClassItem),
    InterfaceMethodRef(RawClassItem),
    String(ConstantPoolIndex),
    NameAndType(RawNameAndType),
    MethodHandle(RawMethodHandle),
    MethodType(RawMethodType),
    InvokeDynamic(RawInvokeDynamic),
}

impl ConstantItem<'_> {
    // Constants from https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4-140

    // Primitives
    const CONSTANT_UTF8: u8 = 1;
    const CONSTANT_INTEGER: u8 = 3;
    const CONSTANT_FLOAT: u8 = 4;
    const CONSTANT_LONG: u8 = 5;
    const CONSTANT_DOUBLE: u8 = 6;

    // Lazy depending
    const CONSTANT_CLASS: u8 = 7;
    const CONSTANT_STRING: u8 = 8;
    const CONSTANT_FIELD_REF: u8 = 9;
    const CONSTANT_METHOD_REF: u8 = 10;
    const CONSTANT_INTERFACE_METHOD_REF: u8 = 11;
    const CONSTANT_NAME_AND_TYPE: u8 = 12;
    const CONSTANT_METHOD_HANDLE: u8 = 15;
    const CONSTANT_METHOD_TYPE: u8 = 16;
    const CONSTANT_INVOKE_DYNAMIC: u8 = 18;
}

#[derive(Debug)]
pub struct RawClass {
    pub name: Utf8Index,
}

/// Unresolved method reference contains a reference
/// to the class, the name and type
#[derive(Debug)]
pub struct RawClassItem {
    /// Reference to the class
    pub class: ClassIndex,
    /// Reference to the name and type
    pub name_and_type: NameAndTypeIndex,
}

#[derive(Debug)]
pub struct RawNameAndType {
    pub name: Utf8Index,
    pub descriptor: Utf8Index,
}

#[derive(Debug)]
pub struct RawMethodHandle {
    pub reference_kind: ReferenceKind,
    pub reference_index: ConstantPoolIndex,
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
pub struct RawMethodType {
    pub descriptor: MethodDescriptorIndex,
}

#[derive(Debug)]
pub struct RawInvokeDynamic {
    pub bootstrap_method_attr_index: u16,
    pub name_and_type: NameAndTypeIndex,
}

#[derive(Debug)]
pub struct RawField<'a> {
    pub access_flags: FieldAccessFlags,
    pub name: Utf8Index,
    pub descriptor: FieldDescriptorIndex,
    pub attributes: Vec<RawAttribute<'a>>,
}

#[derive(Debug)]
pub struct RawAttribute<'a> {
    pub name: Utf8Index,
    pub info: &'a [u8],
}

#[derive(Debug)]
pub struct RawMethod<'a> {
    pub access_flags: MethodAccessFlags,
    pub name: Utf8Index,
    pub descriptor: MethodDescriptorIndex,
    pub attributes: Vec<RawAttribute<'a>>,
}

impl<'a> ByteReadable<'a> for ConstantItem<'a> {
    fn read(r: &mut ByteReader<'a>) -> ReadResult<ConstantItem<'a>> {
        let tag: u8 = r.u1()?;
        Ok(match tag {
            Self::CONSTANT_CLASS => Self::Class(RawClass::read(r)?),
            Self::CONSTANT_FIELD_REF => Self::FieldRef(RawClassItem::read(r)?),
            Self::CONSTANT_METHOD_REF => Self::MethodRef(RawClassItem::read(r)?),
            Self::CONSTANT_INTERFACE_METHOD_REF => Self::InterfaceMethodRef(RawClassItem::read(r)?),
            Self::CONSTANT_STRING => Self::String(ConstantPoolIndex::read(r)?),
            Self::CONSTANT_INTEGER => Self::Integer(i32::read(r)?),
            Self::CONSTANT_FLOAT => Self::Float(f32::read(r)?),
            Self::CONSTANT_LONG => Self::Long(i64::read(r)?),
            Self::CONSTANT_DOUBLE => Self::Double(f64::read(r)?),
            Self::CONSTANT_NAME_AND_TYPE => Self::NameAndType(RawNameAndType::read(r)?),
            Self::CONSTANT_UTF8 => Self::Utf8(<&str as ByteReadable>::read(r)?),
            Self::CONSTANT_METHOD_HANDLE => Self::MethodHandle(RawMethodHandle::read(r)?),
            Self::CONSTANT_METHOD_TYPE => Self::MethodType(RawMethodType::read(r)?),
            Self::CONSTANT_INVOKE_DYNAMIC => Self::InvokeDynamic(RawInvokeDynamic::read(r)?),
            tag => return Err(ReadError::UnknownTag(tag)),
        })
    }
}

impl<'a> ByteReadable<'a> for &'a str {
    fn read(r: &mut ByteReader<'a>) -> ReadResult<Self> {
        let length = r.u2()? as usize;
        let slice = r.slice(length)?;
        let value = str::from_utf8(slice)?;
        Ok(value)
    }
}

impl ByteReadable<'_> for RawNameAndType {
    fn read(r: &mut ByteReader<'_>) -> ReadResult<Self> {
        let name = Utf8Index::read(r)?;
        let descriptor = Utf8Index::read(r)?;
        Ok(Self { name, descriptor })
    }
}

impl ByteReadable<'_> for RawClass {
    fn read(r: &mut ByteReader<'_>) -> ReadResult<Self> {
        let name = Utf8Index::read(r)?;
        Ok(Self { name })
    }
}

impl ByteReadable<'_> for RawClassItem {
    fn read(r: &mut ByteReader<'_>) -> ReadResult<Self> {
        let class = ClassIndex::read(r)?;
        let name_and_type = NameAndTypeIndex::read(r)?;
        Ok(Self {
            class,
            name_and_type,
        })
    }
}

impl ByteReadable<'_> for RawMethodHandle {
    fn read(r: &mut ByteReader<'_>) -> ReadResult<Self> {
        let reference_kind = r.u1()?;
        let reference_kind: ReferenceKind =
            ReferenceKind::from_repr(reference_kind).ok_or(ReadError::UnknownReferenceKind)?;

        let reference_index = ConstantPoolIndex::read(r)?;
        Ok(Self {
            reference_kind,
            reference_index,
        })
    }
}

impl ByteReadable<'_> for RawMethodType {
    fn read(r: &mut ByteReader<'_>) -> ReadResult<Self> {
        let descriptor_index = Utf8Index::read(r)?;
        Ok(Self {
            descriptor: descriptor_index,
        })
    }
}
impl ByteReadable<'_> for RawInvokeDynamic {
    fn read(r: &mut ByteReader<'_>) -> ReadResult<Self> {
        let bootstrap_method_attr_index = r.u2()?;
        let name_and_type_index = NameAndTypeIndex::read(r)?;

        Ok(Self {
            bootstrap_method_attr_index,
            name_and_type: name_and_type_index,
        })
    }
}

impl<'a> ByteReadable<'a> for RawField<'a> {
    fn read(r: &mut ByteReader<'a>) -> ReadResult<Self> {
        let access_flags = FieldAccessFlags::read(r)?;
        let name = Utf8Index::read(r)?;
        let descriptor = Utf8Index::read(r)?;
        let attributes = r.u2_list(false)?;
        Ok(Self {
            access_flags,
            name,
            descriptor,
            attributes,
        })
    }
}

impl<'a> ByteReadable<'a> for RawAttribute<'a> {
    fn read(r: &mut ByteReader<'a>) -> ReadResult<Self> {
        let name_index = ConstantPoolIndex::read(r)?;
        let length = r.u4()? as usize;
        let slice = r.slice(length)?;
        Ok(Self {
            name: name_index,
            info: slice,
        })
    }
}

impl<'a> ByteReadable<'a> for RawMethod<'a> {
    fn read(r: &mut ByteReader<'a>) -> ReadResult<Self> {
        let access_flags = MethodAccessFlags::read(r)?;
        let name = Utf8Index::read(r)?;
        let descriptor = Utf8Index::read(r)?;
        let attributes = r.u2_list(false)?;
        Ok(Self {
            access_flags,
            name,
            descriptor,
            attributes,
        })
    }
}
