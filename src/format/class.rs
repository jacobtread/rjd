use std::{
    fmt::Display,
    str::{self, Utf8Error},
};

use thiserror::Error;

use super::{
    access::{ClassAccessFlags, FieldAccessFlags, MethodAccessFlags},
    reader::{ByteReadable, ByteReader, ReadError},
};

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

    pub fn try_read<'b>(data: &'b [u8]) -> Result<RawClassFile<'b>, ClassReadError> {
        let mut reader = ByteReader::new(data);
        let class_file = RawClassFile::read(&mut reader)?;
        Ok(class_file)
    }
}

#[derive(Debug, Error)]
pub enum ClassReadError {
    #[error("{0}")]
    Read(#[from] ReadError),
    #[error("Class file magic number invalid: Got {0} Expected: {1}")]
    InvalidMagic(u32, u32),
    #[error("Cannot parse unsupported class version: {0}")]
    UnsupportedVersion(SourceVersion),
    #[error("Failed to parse constant pool: {0}")]
    InvalidConstantPool(#[from] ConstantParseError),
}

impl<'a> ByteReadable<'a> for RawClassFile<'a> {
    type Error = ClassReadError;

    fn read(r: &mut ByteReader<'a>) -> Result<Self, Self::Error> {
        let magic = r.u4()?;
        if magic != RawClassFile::MAGIC {
            return Err(ClassReadError::InvalidMagic(magic, Self::MAGIC));
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
    pub values: Vec<RawConstantItem<'a>>,
}

pub trait ConstantPoolResolve<'a>: Sized {
    type Error;

    fn resolve<'b>(
        value: &'b RawConstantItem<'a>,
        pool: &'b ConstantPool<'a>,
    ) -> Result<Self, Self::Error>;
}

impl<'a> ConstantPool<'a> {
    pub fn resolve<'b, R, E>(&'b self, index: &ConstantPoolIndex) -> Option<Result<R, E>>
    where
        R: ConstantPoolResolve<'a, Error = E>,
    {
        let value = self.values.get((*index - 1) as usize)?;
        Some(R::resolve(value, self))
    }
}

impl<'a> ByteReadable<'a> for ConstantPool<'a> {
    type Error = ConstantParseError;

    fn read(r: &mut ByteReader<'a>) -> Result<Self, Self::Error> {
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
    type Error = ReadError;
    fn read(r: &mut ByteReader<'_>) -> Result<Self, Self::Error> {
        let minor = r.u2()?;
        let major = MajorVersion::read(r)?;
        Ok(Self { major, minor })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum MajorVersion {
    JavaLE4,
    Java5,
    Java6,
    Java7,
    Java8,
    Java9,
    Java10,
    Java11,
    Java12,
    Java13,
    Java14,
    Java15,
    Java16,
    Java17,
    Other(u16),
}

impl Display for MajorVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            MajorVersion::JavaLE4 => "Java LE4",
            MajorVersion::Java5 => "Java 5",
            MajorVersion::Java6 => "Java 6",
            MajorVersion::Java7 => "Java 7",
            MajorVersion::Java8 => "Java 8",
            MajorVersion::Java9 => "Java 9",
            MajorVersion::Java10 => "Java 10",
            MajorVersion::Java11 => "Java 11",
            MajorVersion::Java12 => "Java 12",
            MajorVersion::Java13 => "Java 13",
            MajorVersion::Java14 => "Java 14",
            MajorVersion::Java15 => "Java 15",
            MajorVersion::Java16 => "Java 16",
            MajorVersion::Java17 => "Java 17",
            MajorVersion::Other(value) => {
                return write!(f, "Unknown ({})", value);
            }
        })
    }
}

impl From<u16> for MajorVersion {
    fn from(value: u16) -> Self {
        match value {
            48 => Self::JavaLE4,
            49 => Self::Java5,
            50 => Self::Java6,
            51 => Self::Java7,
            52 => Self::Java8,
            53 => Self::Java9,
            54 => Self::Java10,
            55 => Self::Java11,
            56 => Self::Java12,
            57 => Self::Java13,
            58 => Self::Java14,
            59 => Self::Java15,
            60 => Self::Java16,
            61 => Self::Java17,
            value => Self::Other(value),
        }
    }
}

impl ByteReadable<'_> for MajorVersion {
    type Error = ReadError;

    fn read(r: &mut ByteReader<'_>) -> Result<Self, Self::Error> {
        let major = r.u2()?;
        let major = MajorVersion::from(major);
        Ok(major)
    }
}

/// Represents an index within a constant pool
/// that can be resolved to a value

pub type ConstantPoolIndex = u16;

impl ByteReadable<'_> for ConstantPoolIndex {
    type Error = ReadError;

    #[inline]
    fn read<'a>(r: &mut ByteReader<'a>) -> Result<Self, Self::Error> {
        r.u2()
    }
}

/// Unresolved method reference contains a reference
/// to the class, the name and type
#[derive(Debug)]
pub struct RawClassItem {
    /// Reference to the class
    pub class_index: ConstantPoolIndex,
    /// Reference to the name and type
    pub name_and_type_index: ConstantPoolIndex,
}

impl ByteReadable<'_> for RawClassItem {
    type Error = ReadError;

    fn read(r: &mut ByteReader<'_>) -> Result<Self, Self::Error> {
        let class_index = ConstantPoolIndex::read(r)?;
        let name_and_type_index = ConstantPoolIndex::read(r)?;
        Ok(Self {
            class_index,
            name_and_type_index,
        })
    }
}

#[derive(Debug)]
pub struct RawNameAndType {
    pub name_index: ConstantPoolIndex,
    pub descriptor_index: ConstantPoolIndex,
}

impl ByteReadable<'_> for RawNameAndType {
    type Error = ReadError;

    fn read(r: &mut ByteReader<'_>) -> Result<Self, Self::Error> {
        let name_index = ConstantPoolIndex::read(r)?;
        let descriptor_index = ConstantPoolIndex::read(r)?;
        Ok(Self {
            name_index,
            descriptor_index,
        })
    }
}

/// Represents an item within the constant pool that is
/// not yet resolved
#[derive(Debug)]
pub enum RawConstantItem<'a> {
    Class {
        name_index: ConstantPoolIndex,
    },
    FieldRef(RawClassItem),
    MethodRef(RawClassItem),
    InterfaceMethodRef(RawClassItem),
    String(ConstantPoolIndex),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    NameAndType(RawNameAndType),
    Utf8(&'a str),
    MethodHandle {
        reference_kind: ReferenceKind,
        reference_index: ConstantPoolIndex,
    },
    MethodType {
        descriptor_index: ConstantPoolIndex,
    },
    InvokeDynamic {
        bootstrap_method_attr_index: u16,
        name_and_type_index: ConstantPoolIndex,
    },
}

#[derive(Debug)]
pub enum ReferenceKind {
    GetField,
    GetStatic,
    PutField,
    PutStatic,
    InvokeVirtual,
    InvokeStatic,
    InvokeSpecial,
    NewInvokeSpecial,
    InvokeInterface,
}

impl TryFrom<u8> for ReferenceKind {
    type Error = ConstantParseError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            1 => Self::GetField,
            2 => Self::GetStatic,
            3 => Self::PutField,
            4 => Self::PutStatic,
            5 => Self::InvokeVirtual,
            6 => Self::InvokeStatic,
            7 => Self::InvokeSpecial,
            8 => Self::NewInvokeSpecial,
            9 => Self::InvokeInterface,
            _ => return Err(ConstantParseError::UnknownReferenceKind),
        })
    }
}

impl ByteReadable<'_> for ReferenceKind {
    type Error = ConstantParseError;

    fn read(r: &mut ByteReader<'_>) -> Result<Self, Self::Error> {
        let kind = r.u1()?;
        Self::try_from(kind)
    }
}

#[derive(Debug, Error)]
pub enum ConstantParseError {
    #[error("{0}")]
    Read(#[from] ReadError),

    #[error("Unknown constant tag type: {0}")]
    UnknownTag(u8),

    #[error("Invalid utf8: {0}")]
    InvalidUtf8(#[from] Utf8Error),

    #[error("Unknown reference kind")]
    UnknownReferenceKind,
}

impl<'a> ByteReadable<'a> for RawConstantItem<'a> {
    type Error = ConstantParseError;

    fn read(r: &mut ByteReader<'a>) -> Result<RawConstantItem<'a>, Self::Error> {
        let tag = r.u1()?;
        Ok(match tag {
            // CONSTANT_Class
            7 => {
                let name_index = ConstantPoolIndex::read(r)?;
                Self::Class { name_index }
            }
            // CONSTANT_Fieldref
            9 => {
                let item = RawClassItem::read(r)?;
                Self::FieldRef(item)
            }
            // CONSTANT_Methodref
            10 => {
                let item = RawClassItem::read(r)?;
                Self::MethodRef(item)
            }

            // CONSTANT_InterfaceMethodref
            11 => {
                let item = RawClassItem::read(r)?;
                Self::InterfaceMethodRef(item)
            }

            // CONSTANT_String
            8 => {
                let index = ConstantPoolIndex::read(r)?;
                Self::String(index)
            }

            // CONSTANT_Integer
            3 => {
                let value = i32::read(r)?;
                Self::Integer(value)
            }

            // CONSTANT_Float
            4 => {
                let value = f32::read(r)?;
                Self::Float(value)
            }

            // CONSTANT_Long
            5 => {
                let value = i64::read(r)?;
                Self::Long(value)
            }

            // CONSTANT_Double
            6 => {
                let value = f64::read(r)?;
                Self::Double(value)
            }

            // CONSTANT_NameAndType
            12 => {
                let value = RawNameAndType::read(r)?;
                Self::NameAndType(value)
            }

            // CONSTANT_Utf8
            1 => {
                let length = r.u2()? as usize;
                let slice = r.slice(length)?;
                let value = str::from_utf8(slice)?;
                Self::Utf8(value)
            }

            // CONSTANT_MethodHandle
            15 => {
                let reference_kind = ReferenceKind::read(r)?;
                let reference_index = ConstantPoolIndex::read(r)?;
                Self::MethodHandle {
                    reference_kind,
                    reference_index,
                }
            }

            // CONSTANT_MethodType
            16 => {
                let descriptor_index = ConstantPoolIndex::read(r)?;
                Self::MethodType { descriptor_index }
            }

            // CONSTANT_InvokeDynamic
            18 => {
                let bootstrap_method_attr_index = r.u2()?;
                let name_and_type_index = ConstantPoolIndex::read(r)?;
                Self::InvokeDynamic {
                    bootstrap_method_attr_index,
                    name_and_type_index,
                }
            }

            tag => return Err(ConstantParseError::UnknownTag(tag)),
        })
    }
}

#[derive(Debug)]
pub struct RawField<'a> {
    pub access_flags: FieldAccessFlags,
    pub name_index: ConstantPoolIndex,
    pub descriptor_index: ConstantPoolIndex,
    pub attributes: Vec<RawAttribute<'a>>,
}

impl<'a> ByteReadable<'a> for RawField<'a> {
    type Error = ReadError;

    fn read(r: &mut ByteReader<'a>) -> Result<Self, Self::Error> {
        let access_flags = FieldAccessFlags::read(r)?;
        let name_index = ConstantPoolIndex::read(r)?;
        let descriptor_index = ConstantPoolIndex::read(r)?;
        let attributes = r.u2_list(false)?;
        Ok(Self {
            access_flags,
            name_index,
            descriptor_index,
            attributes,
        })
    }
}

#[derive(Debug)]
pub struct RawAttribute<'a> {
    pub name_index: ConstantPoolIndex,
    pub info: &'a [u8],
}

impl<'a> ByteReadable<'a> for RawAttribute<'a> {
    type Error = ReadError;

    fn read(r: &mut ByteReader<'a>) -> Result<Self, Self::Error> {
        let name_index = ConstantPoolIndex::read(r)?;
        let length = r.u4()? as usize;
        let slice = r.slice(length)?;
        Ok(Self {
            name_index,
            info: slice,
        })
    }
}

#[derive(Debug)]
pub struct RawMethod<'a> {
    pub access_flags: MethodAccessFlags,
    pub name_index: ConstantPoolIndex,
    pub descriptor_index: ConstantPoolIndex,
    pub attributes: Vec<RawAttribute<'a>>,
}

impl<'a> ByteReadable<'a> for RawMethod<'a> {
    type Error = ReadError;

    fn read(r: &mut ByteReader<'a>) -> Result<Self, Self::Error> {
        let access_flags = MethodAccessFlags::read(r)?;
        let name_index = ConstantPoolIndex::read(r)?;
        let descriptor_index = ConstantPoolIndex::read(r)?;
        let attributes = r.u2_list(false)?;
        Ok(Self {
            access_flags,
            name_index,
            descriptor_index,
            attributes,
        })
    }
}
