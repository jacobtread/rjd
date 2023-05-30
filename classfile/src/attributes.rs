use strum_macros::FromRepr;

use crate::parser::{constant_pool, AccessFlags};

#[derive(Debug)]
pub enum Attribute<'a> {
    ConstantValue(ConstantValue),
    Code(Code<'a>),
    StackMapTable(StackMapTable),
    Exceptions(Exceptions),
    InnerClasses(InnerClasses),
    EnclosingMethod(EnclosingMethod),
    Synthetic,
    Signature(Signature),
    SourceFile(SourceFile),
    SourceDebugExtension(SourceDebugExtension<'a>),
    LineNumberTable(LineNumberTable),
    LocalVariableTable(LocalVariableTable),
    LocalVariableTypeTable(LocalVariableTypeTable),
    Deprecated,
    RuntimeVisibleAnnotations(RuntimeVisibleAnnotations),
    RuntimeInvisibleAnnotations(RuntimeInvisibleAnnotations),
    RuntimeVisibleParameterAnnotations(RuntimeVisibleParameterAnnotations),
    RuntimeInvisibleParameterAnnotations(RuntimeInvisibleParameterAnnotations),
    RuntimeVisibleTypeAnnotations(RuntimeVisibleTypeAnnotations),
    RuntimeInvisibleTypeAnnotations(RuntimeInvisibleTypeAnnotations),
    AnnotationDefault(ElementValue),
    BootstrapMethods(BootstrapMethods),
    MethodParameters(MethodParameters),
    Other(&'a str, &'a [u8]),
}

#[derive(Debug)]
pub struct ConstantValue {
    pub index: u16,
}

#[derive(Debug)]

pub struct Code<'a> {
    pub max_stack: u16,
    pub max_locals: u16,
    pub code: &'a [u8],
    pub exception_table: Vec<CodeException>,
    pub attributes: Vec<Attribute<'a>>,
}

#[derive(Debug)]
pub struct CodeException {
    pub start_pc: u16,
    pub end_pc: u16,
    pub handler_pc: u16,
    pub catch_type: constant_pool::ClassIndex,
}

#[derive(Debug)]
pub struct StackMapTable {
    pub frames: Vec<StackMapFrame>,
}

#[derive(Debug)]
pub enum StackMapFrame {
    Same {
        offset_delta: u16,
    },
    Same1 {
        offset_delta: u16,
        stack: VerificationType,
    },
    Chop {
        offset_delta: u16,
        count: u8,
    },
    Append {
        offset_delta: u16,
        locals: Vec<VerificationType>,
    },
    Full {
        offset_delta: u16,
        locals: Vec<VerificationType>,
        stack: Vec<VerificationType>,
    },
}

#[derive(Debug)]
pub enum VerificationType {
    Top,
    Integer,
    Float,
    Double,
    Long,
    Null,
    UninitializedThis,
    Object(constant_pool::ClassIndex),
    Uninitialized(u16),
}

#[derive(Debug)]
pub struct Exceptions {
    pub exceptions: Vec<constant_pool::ClassIndex>,
}

#[derive(Debug)]
pub struct InnerClasses {
    pub classes: Vec<InnerClass>,
}

#[derive(Debug)]
pub struct InnerClass {
    pub inner_class: constant_pool::ClassIndex,
    pub outer_class: constant_pool::ClassIndex,
    pub inner_name: constant_pool::Utf8Index,
    pub inner_class_access_flags: AccessFlags,
}

#[derive(Debug)]
pub struct EnclosingMethod {
    pub class: constant_pool::ClassIndex,
    pub method: constant_pool::NameAndTypeIndex,
}

#[derive(Debug)]
pub struct Signature {
    pub signature: constant_pool::Utf8Index,
}

#[derive(Debug)]
pub struct SourceFile {
    pub source_file: constant_pool::Utf8Index,
}

#[derive(Debug)]
pub struct SourceDebugExtension<'a> {
    pub debug_extension: &'a [u8],
}

#[derive(Debug)]
pub struct LineNumberTable {
    pub entires: Vec<LineNumber>,
}

#[derive(Debug)]
pub struct LineNumber {
    pub start_pc: u16,
    pub line_number: u16,
}

#[derive(Debug)]
pub struct LocalVariableTable {
    pub entires: Vec<LocalVariable>,
}

#[derive(Debug)]
pub struct LocalVariable {
    pub start_pc: u16,
    pub length: u16,
    pub name: constant_pool::Utf8Index,
    pub descriptor: constant_pool::DescriptorIndex,
    pub index: u16,
}

#[derive(Debug)]
pub struct LocalVariableTypeTable {
    pub entires: Vec<LocalVariableType>,
}

#[derive(Debug)]
pub struct LocalVariableType {
    pub start_pc: u16,
    pub length: u16,
    pub name_index: constant_pool::Utf8Index,
    pub signature_index: constant_pool::Utf8Index,
    pub index: u16,
}

#[derive(Debug)]
pub struct Annotation {
    pub type_index: constant_pool::Utf8Index,
    pub pairs: AnnotationPair,
}

#[derive(Debug)]
pub struct AnnotationPair {
    pub name: constant_pool::Utf8Index,
    pub value: ElementValue,
}

#[derive(Debug)]
pub enum ElementValue {
    Byte(constant_pool::PoolIndex),
    Char(constant_pool::PoolIndex),
    Double(constant_pool::PoolIndex),
    Float(constant_pool::PoolIndex),
    Int(constant_pool::PoolIndex),
    Long(constant_pool::PoolIndex),
    Short(constant_pool::PoolIndex),
    Boolean(constant_pool::PoolIndex),
    String(constant_pool::PoolIndex),
    Enum(EnumElement),
    Class(constant_pool::Utf8Index),
    Annotation(Box<Annotation>),
    Array(Vec<ElementValue>),
}

#[derive(Debug)]
pub struct EnumElement {
    pub type_name: constant_pool::Utf8Index,
    pub name_index: constant_pool::Utf8Index,
}

#[derive(Debug)]
pub struct RuntimeVisibleAnnotations {
    pub annotations: Vec<Annotation>,
}

#[derive(Debug)]
pub struct RuntimeInvisibleAnnotations {
    pub annotations: Vec<Annotation>,
}

#[derive(Debug)]
pub struct RuntimeVisibleParameterAnnotations {
    pub annotations: Vec<Vec<Annotation>>,
}

#[derive(Debug)]
pub struct RuntimeInvisibleParameterAnnotations {
    pub annotations: Vec<Vec<Annotation>>,
}

#[derive(Debug)]
pub struct TypeAnnotation {
    pub target_type: TargetType,
    pub target_path: Vec<TypePathElement>,
    pub annotation: Annotation,
}

#[derive(Debug)]
pub enum TargetType {
    /// Indicates that an annotation is present
    /// on the type parameter of a class.
    /// The index of the type parameter.
    TypeParameterClass(u8),
    /// Indicates that an annotation is present
    /// on the type parameter of a method.
    /// The index of the type parameter.
    TypeParameterMethod(u8),
    /// Indicates that an annotation is present
    /// on the implements or extends clause of a class.
    /// The index of the super type,
    /// 0xFFFF is the extends clause.
    SuperType(u16),
    /// Indicates that an annotation is present
    /// on a bound of a type parameter of a class.
    TypeParameterBoundClass {
        /// The index of the type parameter.
        type_parameter: u8,
        /// The index of the bound.
        bound_index: u8,
    },
    /// Indicates that an annotation is present
    /// on a bound of a type parameter of a method.
    TypeParameterBoundMethod {
        /// The index of the type parameter.
        type_parameter: u8,
        /// The index of the bound.
        bound_index: u8,
    },
    /// Indicates that an annotation is present
    /// on the type of a field declaration.
    EmptyField,
    /// Indicates that an annotation is present
    /// on the return type of a method
    /// or the type of a newly constructed object.
    EmptyReturn,
    /// Indicates that an annotation is present
    /// on the receiver type of a method.
    EmptyReceiver,
    /// Indicates that an annotation is present
    /// on the type in a formal parameter declaration.
    /// The index of the formal parameter.
    FormalParameter(u8),
    /// Indicates that an annotation is present
    /// on the type in a throws clause.
    /// The index into the table of the `Exceptions` attribute of the method.
    Throws(u16),
    /// Indicates that an annotation is present
    /// on the type in a local variable declaration.
    LocalVariable(Vec<LocalVariableTarget>),
    /// Indicates that an annotation is present
    /// on the type in a local variable declaration.
    ResourceVariable(Vec<LocalVariableTarget>),
    /// Indicates that an annotation is present
    /// on the type in an exception parameter declaration.
    Catch(u16),
    /// Indicates that an annotation is present
    /// on the type in an instanceof expression.
    OffsetInstanceOf(u16),
    /// Indicates that an annotation is present
    /// on the type in a new expression.
    OffsetNew(u16),
    /// Indicates that an annotation is present
    /// on the type before the ::new
    /// of a method reference expression.
    OffsetNewRef(u16),
    /// Indicates that an annotation is present
    /// on the type before the ::name
    /// of a method reference expression.
    OffsetRef(u16),
    /// Indicates that an annotation is present
    /// on the type of a cast expression.
    TypeArgumentCast { offset: u16, type_argument: u8 },
    /// Indicates that an annotation is present
    /// on the type of a method call expression.
    TypeArgumentMethod { offset: u16, type_argument: u8 },
    /// Indicates that an annotation is present
    /// on the type of a new expression.
    TypeArgumentConstructor { offset: u16, type_argument: u8 },
    /// Indicates that an annotation is present
    /// on the type of a ::new expression.
    TypeArgumentNewRef { offset: u16, type_argument: u8 },
    /// Indicates that an annotation is present
    /// on the type of a ::name expression.
    TypeArgumentRef { offset: u16, type_argument: u8 },
}

#[derive(Debug)]
pub struct TypePathElement {
    pub path_kind: TypePathKind,
    pub argument_index: u8,
}

#[derive(Debug, FromRepr)]
pub enum TypePathKind {
    /// Annotation is deeper in an array type
    ArrayType = 0,
    /// Annotation is deeper in a nested type
    NestedType = 1,
    /// Annotation is on the bound of a wildcard type argument of a parameterized type
    WildcardType = 2,
    /// Annotation is on a type argument of a parameterized type
    Type = 3,
}

#[derive(Debug)]
pub struct LocalVariableTarget {
    pub start: u16,
    pub length: u16,
    pub index: u16,
}

#[derive(Debug)]
pub struct RuntimeVisibleTypeAnnotations {
    pub annotations: Vec<TypeAnnotation>,
}

#[derive(Debug)]
pub struct RuntimeInvisibleTypeAnnotations {
    pub annotations: Vec<TypeAnnotation>,
}

#[derive(Debug)]
pub struct BootstrapMethods {
    pub methods: Vec<BootstrapMethod>,
}

#[derive(Debug)]
pub struct BootstrapMethod {
    pub method_ref: constant_pool::PoolIndex,
    pub arguments: Vec<constant_pool::PoolIndex>,
}

#[derive(Debug)]
pub struct MethodParameters {
    pub parameters: Vec<MethodParameter>,
}

#[derive(Debug)]
pub struct MethodParameter {
    pub name: constant_pool::Utf8Index,
    pub access_flags: AccessFlags,
}
