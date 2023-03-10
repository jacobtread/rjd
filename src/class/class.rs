use std::fmt::{Display, Write};

use thiserror::Error;

use crate::format::{
    access::{ClassAccessFlags, FieldAccessFlags, MethodAccessFlags},
    class::{
        ConstantPool, ConstantPoolIndex, ConstantPoolResolve, RawAttribute, RawClassFile,
        RawConstantItem, SourceVersion,
    },
};

use super::constants::{
    FieldType, FieldTypeError, MetDescError, MethodDescriptor, NonUtf8Error, ObjectPath,
    ObjectPathError,
};

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

#[derive(Debug)]
pub struct ClassFile<'a> {
    pub version: SourceVersion,
    pub constant_pool: ConstantPool<'a>,
    pub access_flags: ClassAccessFlags,

    pub this_class: ClassName<'a>,
    pub super_class: Option<ClassName<'a>>,

    pub interfaces: Vec<ClassName<'a>>,

    pub fields: Vec<Field<'a>>,
    pub methods: Vec<Method<'a>>,

    pub attributes: Vec<RawAttribute<'a>>,
}

impl Display for ClassFile<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let this_class = &self.this_class.0;
        let package = &this_class.packages;

        if package.len() > 0 {
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
    ClassNameResolve(#[from] ClassNameResolveError),
    #[error("{0}")]
    ObjectPath(#[from] ObjectPathError),
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
            .resolve(&value.this_class)
            .ok_or_else(|| ClassResolvingError::MissingConstant(value.this_class))??;

        let super_class_index = value.super_class;

        let super_class = if super_class_index == 0 {
            None
        } else {
            Some(
                value
                    .constant_pool
                    .resolve(&super_class_index)
                    .ok_or_else(|| ClassResolvingError::MissingConstant(value.this_class))??,
            )
        };

        let mut interfaces = Vec::with_capacity(value.interfaces.len());

        for interface in value.interfaces {
            let interface = value
                .constant_pool
                .resolve(&super_class_index)
                .ok_or_else(|| ClassResolvingError::MissingConstant(interface))??;

            interfaces.push(interface);
        }

        let mut fields = Vec::with_capacity(value.fields.len());
        for field in value.fields {
            let name = value
                .constant_pool
                .resolve(&field.name_index)
                .ok_or_else(|| ClassResolvingError::MissingConstant(field.name_index))??;
            let descriptor = value
                .constant_pool
                .resolve(&field.descriptor_index)
                .ok_or_else(|| ClassResolvingError::MissingConstant(field.descriptor_index))??;

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
                .resolve(&method.name_index)
                .ok_or_else(|| ClassResolvingError::MissingConstant(method.name_index))??;
            let descriptor = value
                .constant_pool
                .resolve(&method.descriptor_index)
                .ok_or_else(|| ClassResolvingError::MissingConstant(method.descriptor_index))??;

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
pub struct ClassName<'a>(pub ObjectPath<'a>);

#[derive(Debug, Error)]
pub enum ClassNameResolveError {
    #[error("Unexpected constant pool type")]
    UnexpectedType,

    #[error("Missing name field")]
    MissingName,

    #[error("{0}")]
    ObjectPath(#[from] ObjectPathError),
}

impl<'a> ConstantPoolResolve<'a> for ClassName<'a> {
    type Error = ClassNameResolveError;
    fn resolve<'b>(
        value: &RawConstantItem<'a>,
        pool: &'b ConstantPool<'a>,
    ) -> Result<Self, Self::Error> {
        match value {
            RawConstantItem::Class { name_index } => {
                let value: ObjectPath<'a> = pool
                    .resolve(name_index)
                    .ok_or(ClassNameResolveError::MissingName)??;

                Ok(Self(value))
            }
            _ => Err(ClassNameResolveError::UnexpectedType),
        }
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
        values: Vec<ClassName<'a>>,
    },
    InnerClasses {
        inner_class_info: ClassName<'a>,
        outer_class_info: Option<ClassName<'a>>,
        inner_name: &'a str,
        inner_class_access_flags: ClassAccessFlags,
    },
    EnclosingMethod {
        class: ClassName<'a>,
        method_index: ConstantPoolIndex,
    },
    Synthetic,
    Signature(&'a str),
    SourceFile(&'a str),
    SourceDebugExtension(&'a [u8]),
    LineNumberTable,
    LocalVariableTable,
    LocalVariableTypeTable,
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

pub struct LineNumberTableEntry {}
