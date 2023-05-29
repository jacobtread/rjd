use self::attribute::Attribute;

use super::constants::{
    Class, ClassResolveError, FieldType, FieldTypeError, MetDescError, MethodDescriptor,
    NonUtf8Error,
};
use crate::format::{
    access::{ClassAccessFlags, FieldAccessFlags, MethodAccessFlags},
    class::{
        ConstantPool, ConstantPoolIndex, DescriptorIndex, RawAttribute, RawAttributes,
        RawClassFile, SourceVersion, Utf8Index,
    },
};
use thiserror::Error;

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

    pub attributes: Vec<Attribute<'a>>,
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

            let attributes: Vec<Attribute> = field.attributes.parse(&value.constant_pool);

            fields.push(Field {
                access_flags: field.access_flags,
                name,
                descriptor,
                attributes,
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

            let attributes: Vec<Attribute> = method.attributes.parse(&value.constant_pool);

            methods.push(Method {
                access_flags: method.access_flags,
                name,
                descriptor,
                attributes,
            })
        }

        let attributes: Vec<Attribute> = value.attributes.parse(&value.constant_pool);

        Ok(Self {
            version: value.version,
            constant_pool: value.constant_pool,
            access_flags: value.access_flags,
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
pub struct Field<'a> {
    pub access_flags: FieldAccessFlags,
    pub name: &'a str,
    pub descriptor: FieldType,
    pub attributes: Vec<Attribute<'a>>,
}

#[derive(Debug)]
pub struct Method<'a> {
    pub access_flags: MethodAccessFlags,
    pub name: &'a str,
    pub descriptor: MethodDescriptor,
    pub attributes: Vec<Attribute<'a>>,
}

impl<'a> RawAttributes<'a> {
    pub fn parse(self, pool: &ConstantPool<'a>) -> Vec<Attribute<'a>> {
        self.0
            .into_iter()
            .filter_map(|attr| Attribute::try_parse(&attr, pool).ok())
            .collect()
    }
}

mod attribute {
    use thiserror::Error;

    use crate::{
        class::constants::Class,
        format::{
            access::NestedClassAccessFlags,
            class::{
                ClassIndex, ConstantPool, ConstantPoolIndex, ConstantPoolResolve, DescriptorIndex,
                NameAndTypeIndex, RawAttribute, RawAttributes, Utf8Index,
            },
            reader::{ByteReadable, ByteReader, ReadError},
        },
    };

    #[derive(Debug)]
    pub enum Attribute<'a> {
        ConstantValue(ConstantValue),
        Code(Code<'a>),
        StackMapTable(ParserTodo<'a>),
        Exceptions(Exceptions<'a>),
        InnerClasses(InnerClasses),
        EnclosingMethod(EnclosingMethod),
        Synthetic,
        Signature(Signature),
        SourceFile(SourceFile<'a>),
        SourceDebugExtension(SourceDebugExtension<'a>),
        LineNumberTable(LineNumberTable),
        LocalVariableTable(LocalVariableTable),
        LocalVariableTypeTable(LocalVariableTypeTable),
        Deprecated,
        RuntimeVisibleAnnotations(RuntimeVisibleAnnotations),
        RuntimeInvisibleAnnotations(RuntimeInvisibleAnnotations),
        RuntimeVisibleParameterAnnotations(RuntimeVisibleParameterAnnotations),
        RuntimeInvisibleParameterAnnotations(RuntimeInvisibleParameterAnnotations),
        RuntimeVisibleTypeAnnotations(ParserTodo<'a>),
        RuntimeInvisibleTypeAnnotations(ParserTodo<'a>),
        AnnotationDefault(ParserTodo<'a>),
        BootstrapMethods(ParserTodo<'a>),
        MethodParameters(ParserTodo<'a>),
        Other(&'a str, &'a [u8]),
    }

    #[derive(Debug, Error)]
    pub enum AttributeError {
        #[error("No constant value at index: {0}")]
        MissingConstantValue(ConstantPoolIndex),

        #[error(transparent)]
        Read(#[from] ReadError),

        #[error("Attribute name constant was missing or invalid")]
        MissingName,
    }

    trait AttributeParsable<'a>: Sized {
        fn read(r: &mut ByteReader<'a>, pool: &ConstantPool<'a>) -> Result<Self, AttributeError>;
    }

    impl<'a> Attribute<'a> {
        pub fn try_parse(
            attr: &RawAttribute<'a>,
            pool: &ConstantPool<'a>,
        ) -> Result<Attribute<'a>, AttributeError> {
            let name = pool
                .get_utf8(attr.name)
                .ok_or(AttributeError::MissingName)?;
            let r = &mut ByteReader::new(attr.info);

            Ok(match name {
                "ConstantValue" => Attribute::ConstantValue(ConstantValue::read(r, pool)?),
                "Code" => Attribute::Code(Code::read(r, pool)?),
                "SourceFile" => Attribute::SourceFile(SourceFile::read(r, pool)?),
                "Exceptions" => Attribute::Exceptions(Exceptions::read(r, pool)?),
                "LineNumberTable" => Attribute::LineNumberTable(LineNumberTable::read(r, pool)?),

                // TODO: Properly decode
                _ => Attribute::Other(name, attr.info),
            })
        }
    }

    #[derive(Debug)]
    pub struct ConstantValue {
        pub index: u16,
    }

    impl<'a> AttributeParsable<'a> for ConstantValue {
        fn read(r: &mut ByteReader<'a>, pool: &ConstantPool<'a>) -> Result<Self, AttributeError> {
            let index = r.u2()?;

            Ok(Self { index })
        }
    }

    #[derive(Debug)]

    pub struct Code<'a> {
        pub max_stack: u16,
        pub max_locals: u16,
        pub code: &'a [u8],
        pub exception_table: Vec<ExceptionTableEntry>,
        pub attributes: Box<Vec<Attribute<'a>>>,
    }

    impl<'a> AttributeParsable<'a> for Code<'a> {
        fn read(r: &mut ByteReader<'a>, pool: &ConstantPool<'a>) -> Result<Self, AttributeError> {
            let max_stack = r.u2()?;
            let max_locals = r.u2()?;
            let code_length = r.u4()?;
            let code = r.slice(code_length as usize)?;
            let exception_table: Vec<ExceptionTableEntry> = r.u2_list(false)?;
            let attributes: RawAttributes<'a> = RawAttributes(r.u2_list(false)?);
            let attributes: Vec<Attribute<'a>> = attributes.parse(pool);
            Ok(Code {
                max_stack,
                max_locals,
                code,
                exception_table,
                attributes: Box::new(attributes),
            })
        }
    }

    #[derive(Debug)]
    pub struct ExceptionTableEntry {
        pub start_pc: u16,
        pub end_pc: u16,
        pub handler_pc: u16,
        pub catch_type: ConstantPoolIndex,
    }

    impl ByteReadable<'_> for ExceptionTableEntry {
        fn read(r: &mut ByteReader<'_>) -> crate::format::reader::ReadResult<Self> {
            let start_pc = r.u2()?;
            let end_pc = r.u2()?;
            let handler_pc = r.u2()?;
            let catch_type = r.u2()?;
            Ok(Self {
                start_pc,
                end_pc,
                handler_pc,
                catch_type,
            })
        }
    }

    /// TODO: Implement this structure and its parsing
    #[derive(Debug)]
    pub struct ParserTodo<'a>(&'a [u8]);

    #[derive(Debug)]
    pub struct Exceptions<'a> {
        exceptions: Vec<Class<'a>>,
    }

    impl<'a> AttributeParsable<'a> for Exceptions<'a> {
        fn read(r: &mut ByteReader<'a>, pool: &ConstantPool<'a>) -> Result<Self, AttributeError> {
            let exceptions: Vec<ClassIndex> = r.u2_list(false)?;
            let exceptions: Vec<Class<'a>> = exceptions
                .into_iter()
                .filter_map(|index| match pool.get(index) {
                    Some(value) => Class::resolve(value, pool).ok(),
                    None => None,
                })
                .collect();

            Ok(Self { exceptions })
        }
    }

    #[derive(Debug)]
    pub struct InnerClasses {
        classes: Vec<InnerClass>,
    }

    #[derive(Debug)]
    pub struct InnerClass {
        inner_class: ClassIndex,
        outer_class: Option<ClassIndex>,
        inner_name: Option<Utf8Index>,
        inner_class_access_flags: NestedClassAccessFlags,
    }

    #[derive(Debug)]
    pub struct EnclosingMethod {
        class: ClassIndex,
        method: Option<NameAndTypeIndex>,
    }

    #[derive(Debug)]
    pub struct Signature {
        signature: Utf8Index,
    }

    #[derive(Debug)]
    pub struct SourceFile<'a> {
        source_file: &'a str,
    }

    impl<'a> AttributeParsable<'a> for SourceFile<'a> {
        fn read(r: &mut ByteReader<'a>, pool: &ConstantPool<'a>) -> Result<Self, AttributeError> {
            let index = r.u2()?;
            let name = pool.get_utf8(index).ok_or(AttributeError::MissingName)?;
            Ok(SourceFile { source_file: name })
        }
    }

    #[derive(Debug)]
    pub struct SourceDebugExtension<'a> {
        debug_extension: &'a [u8],
    }

    #[derive(Debug)]
    pub struct LineNumberTable {
        entires: Vec<LineNumberTableEntry>,
    }

    impl<'a> AttributeParsable<'a> for LineNumberTable {
        fn read(r: &mut ByteReader<'a>, _pool: &ConstantPool<'a>) -> Result<Self, AttributeError> {
            let entires = r.u2_list(false)?;
            Ok(LineNumberTable { entires })
        }
    }

    #[derive(Debug)]
    pub struct LineNumberTableEntry {
        start_pc: u16,
        line_number: u16,
    }

    impl ByteReadable<'_> for LineNumberTableEntry {
        fn read(r: &mut ByteReader<'_>) -> crate::format::reader::ReadResult<Self> {
            let start_pc = r.u2()?;
            let line_number = r.u2()?;

            Ok(Self {
                start_pc,
                line_number,
            })
        }
    }

    #[derive(Debug)]
    pub struct LocalVariableTable {
        entires: Vec<LocalVariableTableEntry>,
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
    pub struct LocalVariableTypeTable {
        entires: Vec<LocalVariableTypeTableEntry>,
    }

    #[derive(Debug)]
    pub struct LocalVariableTypeTableEntry {
        start_pc: u16,
        length: u16,
        name_index: Utf8Index,
        signature_index: Utf8Index,
        index: u16,
    }

    #[derive(Debug)]
    pub struct Annotation {
        pub type_index: Utf8Index,
        pub pairs: AnnotationPair,
    }

    #[derive(Debug)]
    pub struct AnnotationPair {
        name: Utf8Index,
        value: ElementValue,
    }

    #[derive(Debug)]
    pub enum ElementValue {
        Byte(ConstantPoolIndex),
        Char(ConstantPoolIndex),
        Double(ConstantPoolIndex),
        Float(ConstantPoolIndex),
        Int(ConstantPoolIndex),
        Long(ConstantPoolIndex),
        Short(ConstantPoolIndex),
        Boolean(ConstantPoolIndex),
        String(ConstantPoolIndex),
        Enum(EnumElement),
        Class(Utf8Index),
        Annotation(Box<Annotation>),
        Array(Vec<Box<ElementValue>>),
    }

    #[derive(Debug)]
    pub struct EnumElement {
        type_name: Utf8Index,
        name_index: Utf8Index,
    }

    #[derive(Debug)]
    pub struct RuntimeVisibleAnnotations {
        annotations: Vec<Annotation>,
    }

    #[derive(Debug)]
    pub struct RuntimeInvisibleAnnotations {
        annotations: Vec<Annotation>,
    }

    #[derive(Debug)]
    pub struct RuntimeVisibleParameterAnnotations {
        annotations: Vec<Vec<Annotation>>,
    }

    #[derive(Debug)]
    pub struct RuntimeInvisibleParameterAnnotations {
        annotations: Vec<Vec<Annotation>>,
    }
}
