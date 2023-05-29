use std::{
    fmt::Display,
    str::{self, Utf8Error},
};

use strum_macros::{Display, FromRepr};
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

    pub fn try_read(data: &[u8]) -> Result<RawClassFile<'_>, ClassReadError> {
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
        let major = r.u2()?;

        let major: MajorVersion =
            MajorVersion::from_repr(major).ok_or(ReadError::Other("Unknown major version"))?;

        Ok(Self { major, minor })
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

/// Represents an index within a constant pool
/// that can be resolved to a value

pub type ConstantPoolIndex = u16;

impl ByteReadable<'_> for ConstantPoolIndex {
    type Error = ReadError;

    #[inline]
    fn read(r: &mut ByteReader<'_>) -> Result<Self, Self::Error> {
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

pub enum ConstantItem<'a> {
    /// CONSTANT_Utf8
    Utf8(&'a str),
    /// CONSTANT_Integer
    Integer(i32),
    /// CONSTANT_Float
    Float(f32),
    /// CONSTANT_Long
    Long(i64),
    /// CONSTANT_Double
    Double(f64),
}

impl ConstantItem<'_> {
    const CONSTANT_UTF8: u8 = 1;
    const CONSTANT_INTEGER: u8 = 3;
    const CONSTANT_INTEGER: u8 = 4;
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
                let reference_kind = r.u1()?;
                let reference_kind = ReferenceKind::from_repr(reference_kind)
                    .ok_or(ConstantParseError::UnknownReferenceKind)?;

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
