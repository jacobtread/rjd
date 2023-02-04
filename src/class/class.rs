use thiserror::Error;

use crate::format::{
    access::{ClassAccessFlags, FieldAccessFlags, MethodAccessFlags},
    class::{
        ConstantPool, ConstantPoolResolve, RawAttribute, RawClassFile, RawConstantItem,
        SourceVersion,
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
        dbg!(class_file);
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
