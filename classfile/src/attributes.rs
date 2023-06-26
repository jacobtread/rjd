use std::fmt::Debug;

use nom::{
    combinator::{fail, map, map_parser, map_res, rest},
    multi::{count, length_count, length_data},
    number::complete::{be_u16, be_u32, u8},
    sequence::tuple,
    IResult,
};
use strum_macros::FromRepr;
use thiserror::Error;

use crate::{
    class::{access_flags, AccessFlags},
    constant_pool::{self, ConstantItem, ConstantPool},
    inst::{instruction, Instruction},
};

#[derive(Debug, Error)]
enum AttributeError {
    #[error("Invalid type path: {0}")]
    InvalidTypePath(u8),
    #[error("Attribute name constant was invalid")]
    InvalidAttributeName,
}

#[derive(Debug)]
pub enum Attribute {
    ConstantValue(constant_pool::PoolIndex),
    Code(Code),
    StackMapTable(StackMapTable),
    Exceptions(Exceptions),
    InnerClasses(InnerClasses),
    EnclosingMethod(EnclosingMethod),
    Synthetic,
    Signature(Signature),
    SourceFile(SourceFile),
    SourceDebugExtension(SourceDebugExtension),
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
    Other(String, Vec<u8>),
}

#[derive(Debug)]
pub struct Attributes {
    pub attributes: Vec<Attribute>,
}

pub fn attributes<'b, 'a: 'b>(
    pool: &'b ConstantPool<'a>,
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], Vec<Attribute>> + 'b {
    move |input| length_count(be_u16, attribute(pool))(input)
}

pub fn attribute<'b, 'a: 'b>(
    pool: &'b ConstantPool<'a>,
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], Attribute> + 'b {
    move |input| {
        // The name field of the attribute
        let (input, name) = map_res(be_u16, |value| match pool.get(value) {
            Some(ConstantItem::Utf8(name)) => Ok(*name),
            _ => Err(AttributeError::InvalidAttributeName),
        })(input)?;

        // The actual byte content of the attribute
        let (input, info) = length_data(be_u32)(input)?;

        let (_left, attribute) = match name {
            "ConstantValue" => constant_value(info),
            "Code" => code(pool, info),
            "StackMapTable" => stack_map_table(info),
            "Exceptions" => exceptions(info),
            "InnerClasses" => inner_classes(info),
            "EnclosingMethod" => enclosing_method(info),
            "Synthetic" => return Ok((input, Attribute::Synthetic)),
            "Signature" => signature(info),
            "SourceFile" => source_file(info),
            "SourceDebugExtension" => source_debug_ext(info),
            "LineNumberTable" => line_number_table(info),
            "LocalVariableTable" => local_variable_table(info),
            "LocalVariableTypeTable" => local_variable_type_table(info),
            "Deprecated" => return Ok((input, Attribute::Deprecated)),
            "RuntimeVisibleAnnotations" => runtime_visible_annotations(info),
            "RuntimeInvisibleAnnotations" => runtime_invisible_annotations(info),
            "RuntimeVisibleParameterAnnotations" => runtime_visible_param_annotations(info),
            "RuntimeInvisibleParameterAnnotations" => runtime_invisible_param_annotations(info),
            "RuntimeVisibleTypeAnnotations" => runtime_visible_type_annot(info),
            "RuntimeInvisibleTypeAnnotations" => runtime_invisible_type_annot(info),
            "AnnotationDefault" => annotation_default(info),
            "BootstrapMethods" => bootstrap_methods(info),
            "MethodParameters" => method_parameters(info),
            _ => return Ok((input, Attribute::Other(name.to_string(), info.to_vec()))),
        }?;

        // The remaining data (_left) is discarded

        Ok((input, attribute))
    }
}

fn annotation_default(input: &[u8]) -> IResult<&[u8], Attribute> {
    map(element_value, Attribute::AnnotationDefault)(input)
}

#[derive(Debug)]
pub struct ConstantValue {
    pub index: u16,
}

fn constant_value(input: &[u8]) -> IResult<&[u8], Attribute> {
    map(be_u16, Attribute::ConstantValue)(input)
}

#[derive(Debug)]
pub struct Code {
    pub max_stack: u16,
    pub max_locals: u16,
    pub code: InstructionSeq,
    pub exception_table: Vec<CodeException>,
    pub attributes: Vec<Attribute>,
}

pub type CodeOffset = usize;

/// Collection of instructions and their offset
#[derive(Clone)]
pub struct InstructionSeq {
    /// The actual list of instructions and offsets
    pub inner: Vec<(CodeOffset, Instruction)>,
}

impl Debug for InstructionSeq {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut map = f.debug_map();

        for (offset, instr) in &self.inner {
            map.entry(offset, instr);
        }
        map.finish()
    }
}

fn code_bytes(input: &[u8]) -> IResult<&[u8], InstructionSeq> {
    let mut input = input;
    let mut last_length = input.len();
    let mut pos = 0;

    let mut instructions = Vec::new();

    while last_length > 0 {
        let (i, instruction) = instruction(input, false, pos as i32)?;

        instructions.push((pos, instruction));

        let new_length = i.len();
        // Change in length
        let diff = last_length - new_length;

        input = i;
        last_length = new_length;
        pos += diff;
    }

    let set = InstructionSeq {
        inner: instructions,
    };

    Ok((input, set))
}

#[derive(Debug)]
pub struct CodeException {
    pub start_pc: u16,
    pub end_pc: u16,
    pub handler_pc: u16,
    pub catch_type: constant_pool::ClassIndex,
}

pub fn code<'b, 'a: 'b>(
    pool: &'b ConstantPool<'a>,
    input: &'a [u8],
) -> IResult<&'a [u8], Attribute> {
    map(
        tuple((
            be_u16,
            be_u16,
            map_parser(length_data(be_u32), code_bytes),
            length_count(be_u16, code_exception),
            attributes(pool),
        )),
        |(max_stack, max_locals, code, exception_table, attributes)| {
            Attribute::Code(Code {
                max_stack,
                max_locals,
                code,
                exception_table,
                attributes,
            })
        },
    )(input)
}

fn code_exception(input: &[u8]) -> IResult<&[u8], CodeException> {
    map(
        tuple((be_u16, be_u16, be_u16, be_u16)),
        |(start_pc, end_pc, handler_pc, catch_type)| CodeException {
            start_pc,
            end_pc,
            handler_pc,
            catch_type,
        },
    )(input)
}

#[derive(Debug)]
pub struct StackMapTable {
    pub frames: Vec<StackMapFrame>,
}

fn stack_map_table(input: &[u8]) -> IResult<&[u8], Attribute> {
    map(length_count(be_u16, stack_map_frame), |frames| {
        Attribute::StackMapTable(StackMapTable { frames })
    })(input)
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

fn stack_map_frame(input: &[u8]) -> IResult<&[u8], StackMapFrame> {
    let (input, ty) = u8(input)?;

    match ty {
        0..=63 => stack_map_frame_1(input, ty),
        64..=127 => stack_map_frame_2(input, ty),
        247 => stack_map_frame_3(input),
        248..=250 => stack_map_frame_4(input, ty),
        251 => stack_map_frame_5(input),
        252..=254 => stack_map_frame_6(input, ty),
        255 => stack_map_frame_7(input),
        // TODO: Proper handling of remaining reserved values
        _ => fail(input),
    }
}

fn stack_map_frame_1(input: &[u8], ty: u8) -> IResult<&[u8], StackMapFrame> {
    Ok((
        input,
        StackMapFrame::Same {
            offset_delta: ty as u16,
        },
    ))
}

fn stack_map_frame_2(input: &[u8], ty: u8) -> IResult<&[u8], StackMapFrame> {
    map(verification_type, |verify_type| StackMapFrame::Same1 {
        offset_delta: (ty - 64) as u16,
        stack: verify_type,
    })(input)
}

fn stack_map_frame_3(input: &[u8]) -> IResult<&[u8], StackMapFrame> {
    map(
        tuple((be_u16, verification_type)),
        |(offset_delta, verify_type)| StackMapFrame::Same1 {
            offset_delta,
            stack: verify_type,
        },
    )(input)
}

fn stack_map_frame_4(input: &[u8], ty: u8) -> IResult<&[u8], StackMapFrame> {
    map(be_u16, |offset_delta| StackMapFrame::Chop {
        count: 251 - ty,
        offset_delta,
    })(input)
}

fn stack_map_frame_5(input: &[u8]) -> IResult<&[u8], StackMapFrame> {
    map(be_u16, |offset_delta| StackMapFrame::Same { offset_delta })(input)
}

fn stack_map_frame_6(input: &[u8], ty: u8) -> IResult<&[u8], StackMapFrame> {
    let (input, offset_delta) = be_u16(input)?;
    let diff = (ty - 251) as usize;
    let (input, locals) = count(verification_type, diff)(input)?;

    Ok((
        input,
        StackMapFrame::Append {
            offset_delta,
            locals,
        },
    ))
}

fn stack_map_frame_7(input: &[u8]) -> IResult<&[u8], StackMapFrame> {
    map(
        tuple((
            be_u16,
            length_count(be_u16, verification_type),
            length_count(be_u16, verification_type),
        )),
        |(offset_delta, locals, stack)| StackMapFrame::Full {
            offset_delta,
            locals,
            stack,
        },
    )(input)
}

#[derive(Debug, Clone)]
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

fn verification_type(input: &[u8]) -> IResult<&[u8], VerificationType> {
    let (input, ty) = u8(input)?;
    Ok((
        input,
        (match ty {
            0x0 => VerificationType::Top,
            0x1 => VerificationType::Integer,
            0x2 => VerificationType::Float,
            0x3 => VerificationType::Double,
            0x4 => VerificationType::Long,
            0x5 => VerificationType::Null,
            0x6 => VerificationType::UninitializedThis,
            0x7 => return map(be_u16, VerificationType::Object)(input),
            0x8 => return map(be_u16, VerificationType::Uninitialized)(input),
            // TODO: Proper error
            _ => return fail(input),
        }),
    ))
}

#[derive(Debug)]
pub struct Exceptions {
    pub exceptions: Vec<constant_pool::ClassIndex>,
}

fn exceptions(input: &[u8]) -> IResult<&[u8], Attribute> {
    map(length_count(be_u16, be_u16), |exceptions| {
        Attribute::Exceptions(Exceptions { exceptions })
    })(input)
}

#[derive(Debug)]
pub struct InnerClasses {
    pub classes: Vec<InnerClass>,
}

fn inner_classes(input: &[u8]) -> IResult<&[u8], Attribute> {
    map(length_count(be_u16, inner_class), |classes| {
        Attribute::InnerClasses(InnerClasses { classes })
    })(input)
}

#[derive(Debug)]
pub struct InnerClass {
    pub inner_class: constant_pool::ClassIndex,
    pub outer_class: constant_pool::ClassIndex,
    pub inner_name: constant_pool::Utf8Index,
    pub inner_class_access_flags: AccessFlags,
}

fn inner_class(input: &[u8]) -> IResult<&[u8], InnerClass> {
    map(
        tuple((be_u16, be_u16, be_u16, be_u16)),
        |(inner_class, outer_class, inner_name, inner_class_access_flags)| InnerClass {
            inner_class,
            outer_class,
            inner_name,
            inner_class_access_flags: AccessFlags::from_bits_retain(inner_class_access_flags),
        },
    )(input)
}

#[derive(Debug)]
pub struct EnclosingMethod {
    pub class: constant_pool::ClassIndex,
    pub method: constant_pool::NameAndTypeIndex,
}

fn enclosing_method(input: &[u8]) -> IResult<&[u8], Attribute> {
    map(tuple((be_u16, be_u16)), |(class, method)| {
        Attribute::EnclosingMethod(EnclosingMethod { class, method })
    })(input)
}

#[derive(Debug)]
pub struct Signature {
    pub signature: constant_pool::Utf8Index,
}

fn signature(input: &[u8]) -> IResult<&[u8], Attribute> {
    map(be_u16, |signature| {
        Attribute::Signature(Signature { signature })
    })(input)
}

#[derive(Debug)]
pub struct SourceFile {
    pub source_file: constant_pool::Utf8Index,
}

fn source_file(input: &[u8]) -> IResult<&[u8], Attribute> {
    map(be_u16, |source_file| {
        Attribute::SourceFile(SourceFile { source_file })
    })(input)
}

#[derive(Debug)]
pub struct SourceDebugExtension {
    pub debug_extension: Vec<u8>,
}

fn source_debug_ext(input: &[u8]) -> IResult<&[u8], Attribute> {
    map(rest, |debug_extension: &[u8]| {
        Attribute::SourceDebugExtension(SourceDebugExtension {
            debug_extension: debug_extension.to_vec(),
        })
    })(input)
}

pub struct LineNumberTable {
    pub entries: Vec<LineNumber>,
}

impl Debug for LineNumberTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut map = f.debug_map();

        for line in &self.entries {
            map.entry(&line.start_pc, &line.line_number);
        }
        map.finish()
    }
}

fn line_number_table(input: &[u8]) -> IResult<&[u8], Attribute> {
    map(length_count(be_u16, line_number), |entries| {
        Attribute::LineNumberTable(LineNumberTable { entries })
    })(input)
}

#[derive(Debug)]
pub struct LineNumber {
    pub start_pc: u16,
    pub line_number: u16,
}

fn line_number(input: &[u8]) -> IResult<&[u8], LineNumber> {
    map(tuple((be_u16, be_u16)), |(start_pc, line_number)| {
        LineNumber {
            start_pc,
            line_number,
        }
    })(input)
}

#[derive(Debug)]
pub struct LocalVariableTable {
    pub entries: Vec<LocalVariable>,
}

fn local_variable_table(input: &[u8]) -> IResult<&[u8], Attribute> {
    map(length_count(be_u16, local_variable), |entries| {
        Attribute::LocalVariableTable(LocalVariableTable { entries })
    })(input)
}

#[derive(Debug)]
pub struct LocalVariable {
    pub start_pc: u16,
    pub length: u16,
    pub name: constant_pool::Utf8Index,
    pub descriptor: constant_pool::DescriptorIndex,
    pub index: u16,
}

fn local_variable(input: &[u8]) -> IResult<&[u8], LocalVariable> {
    map(
        tuple((be_u16, be_u16, be_u16, be_u16, be_u16)),
        |(start_pc, length, name, descriptor, index)| LocalVariable {
            start_pc,
            length,
            name,
            descriptor,
            index,
        },
    )(input)
}

#[derive(Debug)]
pub struct LocalVariableTypeTable {
    pub entries: Vec<LocalVariableType>,
}

fn local_variable_type_table(input: &[u8]) -> IResult<&[u8], Attribute> {
    map(length_count(be_u16, local_variable_type), |entries| {
        Attribute::LocalVariableTypeTable(LocalVariableTypeTable { entries })
    })(input)
}

#[derive(Debug)]
pub struct LocalVariableType {
    pub start_pc: u16,
    pub length: u16,
    pub name: constant_pool::Utf8Index,
    pub signature: constant_pool::Utf8Index,
    pub index: u16,
}

fn local_variable_type(input: &[u8]) -> IResult<&[u8], LocalVariableType> {
    map(
        tuple((be_u16, be_u16, be_u16, be_u16, be_u16)),
        |(start_pc, length, name, signature, index)| LocalVariableType {
            start_pc,
            length,
            name,
            signature,
            index,
        },
    )(input)
}

#[derive(Debug)]
pub struct Annotation {
    pub type_index: constant_pool::Utf8Index,
    pub values: Vec<NamedElementValue>,
}

fn annotation(input: &[u8]) -> IResult<&[u8], Annotation> {
    map(
        tuple((
            // Type Index
            be_u16,
            // Element values
            length_count(be_u16, named_element_value),
        )),
        |(type_index, values)| Annotation { type_index, values },
    )(input)
}

#[derive(Debug)]
pub struct NamedElementValue {
    pub name: constant_pool::Utf8Index,
    pub value: ElementValue,
}

fn named_element_value(input: &[u8]) -> IResult<&[u8], NamedElementValue> {
    map(tuple((be_u16, element_value)), |(name, value)| {
        NamedElementValue { name, value }
    })(input)
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

fn element_value(input: &[u8]) -> IResult<&[u8], ElementValue> {
    let (input, tag) = u8(input)?;

    match tag {
        b'B' => map(be_u16, ElementValue::Byte)(input),
        b'C' => map(be_u16, ElementValue::Char)(input),
        b'D' => map(be_u16, ElementValue::Double)(input),
        b'F' => map(be_u16, ElementValue::Float)(input),
        b'I' => map(be_u16, ElementValue::Int)(input),
        b'J' => map(be_u16, ElementValue::Long)(input),
        b'S' => map(be_u16, ElementValue::Short)(input),
        b'Z' => map(be_u16, ElementValue::Boolean)(input),
        b's' => map(be_u16, ElementValue::String)(input),
        b'e' => element_value_enum(input),
        b'c' => map(be_u16, ElementValue::Class)(input),
        b'@' => element_value_annot(input),
        b'[' => element_value_array(input),
        // TODO: Properly handle
        _ => fail(input),
    }
}

fn element_value_enum(input: &[u8]) -> IResult<&[u8], ElementValue> {
    map(tuple((be_u16, be_u16)), |(type_name, name_index)| {
        ElementValue::Enum(EnumElement {
            type_name,
            name_index,
        })
    })(input)
}

fn element_value_annot(input: &[u8]) -> IResult<&[u8], ElementValue> {
    map(annotation, |annotation| {
        ElementValue::Annotation(Box::new(annotation))
    })(input)
}

fn element_value_array(input: &[u8]) -> IResult<&[u8], ElementValue> {
    map(length_count(be_u16, element_value), ElementValue::Array)(input)
}

fn annotations(input: &[u8]) -> IResult<&[u8], Vec<Annotation>> {
    length_count(be_u16, annotation)(input)
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

fn runtime_visible_annotations(input: &[u8]) -> IResult<&[u8], Attribute> {
    map(annotations, |annotations| {
        Attribute::RuntimeVisibleAnnotations(RuntimeVisibleAnnotations { annotations })
    })(input)
}

#[derive(Debug)]
pub struct RuntimeInvisibleAnnotations {
    pub annotations: Vec<Annotation>,
}

fn runtime_invisible_annotations(input: &[u8]) -> IResult<&[u8], Attribute> {
    map(annotations, |annotations| {
        Attribute::RuntimeVisibleAnnotations(RuntimeVisibleAnnotations { annotations })
    })(input)
}

#[derive(Debug)]
pub struct RuntimeVisibleParameterAnnotations {
    pub annotations: Vec<Vec<Annotation>>,
}

fn runtime_visible_param_annotations(input: &[u8]) -> IResult<&[u8], Attribute> {
    map(length_count(be_u16, annotations), |annotations| {
        Attribute::RuntimeVisibleParameterAnnotations(RuntimeVisibleParameterAnnotations {
            annotations,
        })
    })(input)
}

#[derive(Debug)]
pub struct RuntimeInvisibleParameterAnnotations {
    pub annotations: Vec<Vec<Annotation>>,
}

fn runtime_invisible_param_annotations(input: &[u8]) -> IResult<&[u8], Attribute> {
    map(length_count(be_u16, annotations), |annotations| {
        Attribute::RuntimeInvisibleParameterAnnotations(RuntimeInvisibleParameterAnnotations {
            annotations,
        })
    })(input)
}

#[derive(Debug)]
pub struct TypeAnnotation {
    pub target_type: TargetType,
    pub target_path: Vec<TypePathElement>,
    pub annotation: Annotation,
}

fn type_annotation(input: &[u8]) -> IResult<&[u8], TypeAnnotation> {
    map(
        tuple((target_type, type_path_elements, annotation)),
        |(target_type, target_path, annotation)| TypeAnnotation {
            target_type,
            target_path,
            annotation,
        },
    )(input)
}

fn target_type(input: &[u8]) -> IResult<&[u8], TargetType> {
    use TargetType::*;
    let (input, ty) = u8(input)?;

    match ty {
        0x00 => map(u8, TypeParameterClass)(input),
        0x01 => map(u8, TypeParameterMethod)(input),
        0x10 => map(be_u16, SuperType)(input),
        0x11 => map(tuple((u8, u8)), |(type_parameter, bound_index)| {
            TypeParameterBoundClass {
                type_parameter,
                bound_index,
            }
        })(input),
        0x12 => map(tuple((u8, u8)), |(type_parameter, bound_index)| {
            TypeParameterBoundMethod {
                type_parameter,
                bound_index,
            }
        })(input),
        0x13 => Ok((input, EmptyField)),
        0x14 => Ok((input, EmptyReturn)),
        0x15 => Ok((input, EmptyReceiver)),
        0x16 => map(u8, FormalParameter)(input),
        0x17 => map(be_u16, Throws)(input),
        0x40 => map(local_variable_targets, LocalVariable)(input),
        0x41 => map(local_variable_targets, ResourceVariable)(input),
        0x42 => map(be_u16, Catch)(input),
        0x43 => map(be_u16, OffsetInstanceOf)(input),
        0x44 => map(be_u16, OffsetNew)(input),
        0x45 => map(be_u16, OffsetNewRef)(input),
        0x46 => map(be_u16, OffsetRef)(input),
        0x47 => map(tuple((be_u16, u8)), |(offset, type_argument)| {
            TypeArgumentCast {
                offset,
                type_argument,
            }
        })(input),
        0x48 => map(tuple((be_u16, u8)), |(offset, type_argument)| {
            TypeArgumentConstructor {
                offset,
                type_argument,
            }
        })(input),
        0x49 => map(tuple((be_u16, u8)), |(offset, type_argument)| {
            TypeArgumentMethod {
                offset,
                type_argument,
            }
        })(input),
        0x4A => map(tuple((be_u16, u8)), |(offset, type_argument)| {
            TypeArgumentNewRef {
                offset,
                type_argument,
            }
        })(input),
        0x4B => map(tuple((be_u16, u8)), |(offset, type_argument)| {
            TypeArgumentRef {
                offset,
                type_argument,
            }
        })(input),
        _ => fail(input),
    }
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

fn type_path_elements(input: &[u8]) -> IResult<&[u8], Vec<TypePathElement>> {
    length_count(u8, type_path_element)(input)
}

#[derive(Debug)]
pub struct TypePathElement {
    pub path_kind: TypePathKind,
    pub argument_index: u8,
}

fn type_path_element(input: &[u8]) -> IResult<&[u8], TypePathElement> {
    map(
        tuple((type_path_kind, u8)),
        |(path_kind, argument_index)| TypePathElement {
            path_kind,
            argument_index,
        },
    )(input)
}

#[derive(Debug, FromRepr)]
#[repr(u8)]
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

fn type_path_kind(input: &[u8]) -> IResult<&[u8], TypePathKind> {
    map_res(u8, |value| {
        TypePathKind::from_repr(value).ok_or(AttributeError::InvalidTypePath(value))
    })(input)
}

fn local_variable_targets(input: &[u8]) -> IResult<&[u8], Vec<LocalVariableTarget>> {
    length_count(be_u16, local_variable_target)(input)
}

#[derive(Debug)]
pub struct LocalVariableTarget {
    pub start: u16,
    pub length: u16,
    pub index: u16,
}

fn local_variable_target(input: &[u8]) -> IResult<&[u8], LocalVariableTarget> {
    map(tuple((be_u16, be_u16, be_u16)), |(start, length, index)| {
        LocalVariableTarget {
            start,
            length,
            index,
        }
    })(input)
}

#[derive(Debug)]
pub struct RuntimeVisibleTypeAnnotations {
    pub annotations: Vec<TypeAnnotation>,
}

fn runtime_visible_type_annot(input: &[u8]) -> IResult<&[u8], Attribute> {
    map(length_count(be_u16, type_annotation), |annotations| {
        Attribute::RuntimeVisibleTypeAnnotations(RuntimeVisibleTypeAnnotations { annotations })
    })(input)
}

#[derive(Debug)]
pub struct RuntimeInvisibleTypeAnnotations {
    pub annotations: Vec<TypeAnnotation>,
}

fn runtime_invisible_type_annot(input: &[u8]) -> IResult<&[u8], Attribute> {
    map(length_count(be_u16, type_annotation), |annotations| {
        Attribute::RuntimeInvisibleTypeAnnotations(RuntimeInvisibleTypeAnnotations { annotations })
    })(input)
}

#[derive(Debug)]
pub struct BootstrapMethods {
    pub methods: Vec<BootstrapMethod>,
}

fn bootstrap_methods(input: &[u8]) -> IResult<&[u8], Attribute> {
    map(length_count(be_u16, bootstrap_method), |methods| {
        Attribute::BootstrapMethods(BootstrapMethods { methods })
    })(input)
}

#[derive(Debug)]
pub struct BootstrapMethod {
    pub method_ref: constant_pool::PoolIndex,
    pub arguments: Vec<constant_pool::PoolIndex>,
}

fn bootstrap_method(input: &[u8]) -> IResult<&[u8], BootstrapMethod> {
    map(
        tuple((be_u16, length_count(be_u16, be_u16))),
        |(method_ref, arguments)| BootstrapMethod {
            method_ref,
            arguments,
        },
    )(input)
}

#[derive(Debug)]
pub struct MethodParameters {
    pub parameters: Vec<MethodParameter>,
}

fn method_parameters(input: &[u8]) -> IResult<&[u8], Attribute> {
    map(length_count(be_u16, method_parameter), |parameters| {
        Attribute::MethodParameters(MethodParameters { parameters })
    })(input)
}

#[derive(Debug)]
pub struct MethodParameter {
    pub name: constant_pool::Utf8Index,
    pub access_flags: AccessFlags,
}

fn method_parameter(input: &[u8]) -> IResult<&[u8], MethodParameter> {
    map(tuple((be_u16, access_flags)), |(name, access_flags)| {
        MethodParameter { name, access_flags }
    })(input)
}
