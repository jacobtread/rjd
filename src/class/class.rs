use std::fmt::{Display, Write};

use thiserror::Error;

use crate::format::{
    access::{ClassAccessFlags, FieldAccessFlags, MethodAccessFlags},
    class::{ConstantPool, ConstantPoolIndex, RawAttribute, RawClassFile, SourceVersion},
};

use super::constants::{
    Class, ClassResolveError, FieldType, FieldTypeError, MetDescError, MethodDescriptor,
    NonUtf8Error,
};

#[derive(Debug)]
pub struct ClassFile<'a> {
    pub version: SourceVersion,
    pub constant_pool: ConstantPool<'a>,
    pub access_flags: ClassAccessFlags,

    pub this_class: Class<'a>,
    pub super_class: Option<Class<'a>>,

    pub interfaces: Vec<Class<'a>>,

    pub fields: Vec<Field<'a>>,
    pub methods: Vec<Method<'a>>,

    pub attributes: Vec<RawAttribute<'a>>,
}

impl Display for ClassFile<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let this_class = &self.this_class;
        let package = &this_class.packages;

        if !package.is_empty() {
            writeln!(f, "package {};", package.join("."))?;
        }

        f.write_char('\n')?;

        self.access_flags.fmt(f)?;

        f.write_char(' ')?;
        f.write_str(this_class.class)?;
        f.write_str(" {\n\n")?;

        for field in &self.fields {
            f.write_str("  ")?;
            field.access_flags.fmt(f)?;
            field.descriptor.fmt(f)?;
            f.write_char(' ')?;
            f.write_str(field.name)?;
            f.write_str(";\n")?;
        }
        f.write_char('\n')?;

        f.write_str("}")?;

        Ok(())
    }
}

#[derive(Debug, Error)]
pub enum ClassResolvingError {
    #[error("Missing constant pool value: {0}")]
    MissingConstant(u16),
    #[error("{0}")]
    ClassNameResolve(#[from] ClassResolveError),

    #[error("{0}")]
    NonUtf8(#[from] NonUtf8Error),
    #[error("{0}")]
    MetDesc(#[from] MetDescError),
    #[error("{0}")]
    FieldType(#[from] FieldTypeError),
}

impl<'a> TryFrom<RawClassFile<'a>> for ClassFile<'a> {
    type Error = ClassResolvingError;

    fn try_from(value: RawClassFile<'a>) -> Result<Self, Self::Error> {
        let this_class = value
            .constant_pool
            .resolve(value.this_class)
            .ok_or(ClassResolvingError::MissingConstant(value.this_class))??;

        let super_class_index = value.super_class;

        let super_class = if super_class_index == 0 {
            None
        } else {
            Some(
                value
                    .constant_pool
                    .resolve(super_class_index)
                    .ok_or(ClassResolvingError::MissingConstant(value.this_class))??,
            )
        };

        let mut interfaces = Vec::with_capacity(value.interfaces.len());

        for interface in value.interfaces {
            let interface = value
                .constant_pool
                .resolve(super_class_index)
                .ok_or(ClassResolvingError::MissingConstant(interface))??;

            interfaces.push(interface);
        }

        let mut fields = Vec::with_capacity(value.fields.len());
        for field in value.fields {
            let name = value
                .constant_pool
                .resolve(field.name)
                .ok_or(ClassResolvingError::MissingConstant(field.name))??;
            let descriptor = value
                .constant_pool
                .resolve(field.descriptor)
                .ok_or(ClassResolvingError::MissingConstant(field.descriptor))??;

            fields.push(Field {
                access_flags: field.access_flags,
                name,
                descriptor,
                attributes: field.attributes,
            })
        }

        let mut methods = Vec::with_capacity(value.methods.len());
        for method in value.methods {
            let name = value
                .constant_pool
                .resolve(method.name)
                .ok_or(ClassResolvingError::MissingConstant(method.name))??;
            let descriptor = value
                .constant_pool
                .resolve(method.descriptor)
                .ok_or(ClassResolvingError::MissingConstant(method.descriptor))??;

            methods.push(Method {
                access_flags: method.access_flags,
                name,
                descriptor,
                attributes: method.attributes,
            })
        }

        Ok(Self {
            version: value.version,
            constant_pool: value.constant_pool,
            access_flags: value.access_flags,
            this_class,
            super_class,
            interfaces,
            fields,
            methods,
            attributes: value.attributes,
        })
    }
}

#[derive(Debug)]
pub struct Field<'a> {
    pub access_flags: FieldAccessFlags,
    pub name: &'a str,
    pub descriptor: FieldType,
    pub attributes: Vec<RawAttribute<'a>>,
}

#[derive(Debug)]
pub struct Method<'a> {
    pub access_flags: MethodAccessFlags,
    pub name: &'a str,
    pub descriptor: MethodDescriptor,
    pub attributes: Vec<RawAttribute<'a>>,
}

#[derive(Debug)]
pub enum Attribute<'a> {
    ConstantValue(ConstantPoolIndex),
    Code(Code<'a>),
    StackMapTable {
        bytes: &'a [u8],
    },
    Exceptions {
        values: Vec<Class<'a>>,
    },
    InnerClasses {
        inner_class_info: Class<'a>,
        outer_class_info: Option<Class<'a>>,
        inner_name: &'a str,
        inner_class_access_flags: ClassAccessFlags,
    },
    EnclosingMethod {
        class: Class<'a>,
        method_index: ConstantPoolIndex,
    },
    Synthetic,
    Signature(&'a str),
    SourceFile(&'a str),
    SourceDebugExtension(&'a [u8]),
    LineNumberTable {
        values: Vec<LineNumberTableEntry>,
    },
    LocalVariableTable {
        values: Vec<LocalVariableTableEntry>,
    },
    LocalVariableTypeTable {
        values: Vec<LocalVariableTypeTableEntry>,
    },
    Deprecated,
    RuntimeVisibleAnnotations,
    RuntimeInvisibleAnnotations,
    RuntimeVisibleParameterAnnotations,
    RuntimeInvisibleParameterAnnotations,
    RuntimeVisibleTypeAnnotations,
    RuntimeInvisibleTypeAnnotations,
    AnnotationDefault,
    BootstrapMethods,
    MethodParameters,

    Other(&'a str, &'a [u8]),
}

/// Index that can be resolved in a constant pool to gain a Utf8 value
#[derive(Debug)]
pub struct Utf8Index(ConstantPoolIndex);

/// Index that can be resolved in a constant pool to gain a Utf8 value
/// that can then be decoded into a descriptor
#[derive(Debug)]
pub struct DescriptorIndex(ConstantPoolIndex);

#[derive(Debug)]

pub struct Code<'a> {
    pub max_stack: u16,
    pub max_locals: u16,
    pub code: &'a [u8],
    pub exception_table: Vec<ExceptionTableEntry>,
    pub attributes: Box<Vec<Attribute<'a>>>,
}

#[derive(Debug)]
pub struct ExceptionTableEntry {
    pub start_pc: u16,
    pub end_pc: u16,
    pub handler_pc: u16,
    pub catch_type: ConstantPoolIndex,
}

#[derive(Debug)]
pub struct LineNumberTableEntry {
    start_pc: u16,
    line_number: u16,
}

#[derive(Debug)]
pub struct LocalVariableTableEntry {
    start_pc: u16,
    length: u16,
    name_index: Utf8Index,
    descriptor_index: DescriptorIndex,
    index: u16,
}

#[derive(Debug)]
pub struct LocalVariableTypeTableEntry {
    start_pc: u16,
    length: u16,
    name_index: Utf8Index,
    signature_index: Utf8Index,
    index: u16,
}

pub struct Annotation {
    type_index: Utf8Index,
}

#[cfg(test)]
mod test {
    use std::fs::read;

    use crate::class::class::ClassFile;

    use super::RawClassFile;

    #[test]
    fn test_parse_class() {
        let file = read("Test.class").unwrap();
        let class_file = RawClassFile::try_read(&file).unwrap();
        let class_file = ClassFile::try_from(class_file).unwrap();
        println!("{}", class_file)
    }
}
