use nom::{
    combinator::{fail, map, map_res},
    multi::{count, length_data},
    number::streaming::{be_f32, be_f64, be_i32, be_i64, be_u16, be_u8, u8},
    sequence::tuple,
    IResult,
};
use strum_macros::FromRepr;
use thiserror::Error;

use crate::types::{field_descriptor, method_descriptor, Class, FieldDesc, MethodDescriptor};

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

    pub fn get_utf8(&self, index: PoolIndex) -> Option<&'a str> {
        match self.get(index) {
            Some(ConstantItem::Utf8(value)) => Some(*value),
            _ => None,
        }
    }

    pub fn get_name_and_type(&self, index: PoolIndex) -> Option<NameAndType> {
        match self.get(index) {
            Some(ConstantItem::NameAndType(value)) => Some(value.clone()),
            _ => None,
        }
    }

    pub fn get_methodref(&self, index: PoolIndex) -> Option<Methodref<'a>> {
        let class_item = match self.get(index) {
            Some(ConstantItem::Methodref(value)) => value,
            //TODO: Hacky might not be right
            Some(ConstantItem::InterfaceMethodref(value)) => value,
            _ => return None,
        };

        let class: Class<'a> = Class::try_parse(self.get_class_name(class_item.class)?)?;
        let name_and_type = self.get_name_and_type(class_item.name_and_type)?;
        let name_and_type = self.get_method_name_and_type(name_and_type)?;
        Some(Methodref {
            class,
            name: name_and_type.name,
            descriptor: name_and_type.descriptor,
        })
    }

    pub fn get_fieldref(&self, index: PoolIndex) -> Option<Fieldref<'a>> {
        let class_item = match self.get(index) {
            Some(ConstantItem::Fieldref(value)) => value,
            _ => return None,
        };

        let class: Class<'a> = Class::try_parse(self.get_class_name(class_item.class)?)?;
        let name_and_type = self.get_name_and_type(class_item.name_and_type)?;
        let name_and_type = self.get_field_name_and_type(name_and_type)?;
        Some(Fieldref {
            class,
            name: name_and_type.name,
            descriptor: name_and_type.descriptor,
        })
    }

    pub fn get_class_name(&self, index: PoolIndex) -> Option<&'a str> {
        let index = match self.get(index) {
            Some(ConstantItem::Class(value)) => value.name,
            _ => return None,
        };
        self.get_utf8(index)
    }

    pub fn get_field_name_and_type(
        &self,
        name_and_type: NameAndType,
    ) -> Option<FieldNameAndType<'a>> {
        let name = self.get_utf8(name_and_type.name)?;
        let descriptor = self.get_utf8(name_and_type.descriptor)?;
        // TODO: Handling invalid descriptors
        let (_, descriptor) = field_descriptor(descriptor).ok()?;
        Some(FieldNameAndType { name, descriptor })
    }

    pub fn get_method_name_and_type(
        &self,
        name_and_type: NameAndType,
    ) -> Option<MethodNameAndType<'a>> {
        let name = self.get_utf8(name_and_type.name)?;
        let descriptor = self.get_utf8(name_and_type.descriptor)?;
        // TODO: Handling invalid descriptors
        let (_, descriptor) = method_descriptor(descriptor).ok()?;
        Some(MethodNameAndType { name, descriptor })
    }

    pub fn get_invokedynamic(&self, index: PoolIndex) -> Option<InvokeDynamic> {
        match self.get(index) {
            Some(ConstantItem::InvokeDynamic(value)) => Some(value.clone()),
            _ => None,
        }
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

pub fn constant_pool(input: &[u8]) -> IResult<&[u8], ConstantPool<'_>> {
    let (input, length) = be_u16(input)?;
    let length = (length - 1) as usize;
    let (input, table) = count(constant_item, length)(input)?;
    Ok((input, ConstantPool { table }))
}

fn constant_item(input: &[u8]) -> IResult<&[u8], ConstantItem<'_>> {
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
        UTF8 => utf8(input),

        INTEGER => map(be_i32, ConstantItem::Integer)(input),
        LONG => map(be_i64, ConstantItem::Long)(input),

        FLOAT => map(be_f32, ConstantItem::Float)(input),
        DOUBLE => map(be_f64, ConstantItem::Double)(input),

        CLASS => class_info(input),
        STRING => map(be_u16, ConstantItem::String)(input),

        FIELDREF => map(class_item, ConstantItem::Fieldref)(input),
        METHODREF => map(class_item, ConstantItem::Methodref)(input),
        INTERFACE_METHODREF => map(class_item, ConstantItem::InterfaceMethodref)(input),

        NAME_AND_TYPE => name_and_type(input),

        METHOD_HANDLE => method_handle(input),
        METHOD_TYPE => method_type(input),

        INVOKE_DYNAMIC => invoke_dynamic(input),

        // TODO: Proper error
        _ => fail(input),
    }
}

fn utf8(input: &[u8]) -> IResult<&[u8], ConstantItem<'_>> {
    map_res(length_data(be_u16), |bytes| {
        std::str::from_utf8(bytes).map(ConstantItem::Utf8)
    })(input)
}

fn class_info(input: &[u8]) -> IResult<&[u8], ConstantItem<'_>> {
    map(be_u16, |name| ConstantItem::Class(ClassInfo { name }))(input)
}

fn class_item(input: &[u8]) -> IResult<&[u8], ClassItem> {
    map(tuple((be_u16, be_u16)), |(class, name_and_type)| {
        ClassItem {
            class,
            name_and_type,
        }
    })(input)
}

fn name_and_type(input: &[u8]) -> IResult<&[u8], ConstantItem<'_>> {
    map(tuple((be_u16, be_u16)), |(name, descriptor)| {
        ConstantItem::NameAndType(NameAndType { name, descriptor })
    })(input)
}

fn method_type(input: &[u8]) -> IResult<&[u8], ConstantItem<'_>> {
    map(be_u16, |descriptor| {
        ConstantItem::MethodType(MethodType { descriptor })
    })(input)
}

fn method_handle(input: &[u8]) -> IResult<&[u8], ConstantItem<'_>> {
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

fn invoke_dynamic(input: &[u8]) -> IResult<&[u8], ConstantItem<'_>> {
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
#[derive(Debug, Clone)]
pub struct ClassItem {
    pub class: ClassIndex,
    pub name_and_type: NameAndTypeIndex,
}

#[derive(Debug, Clone)]
pub struct Methodref<'a> {
    pub class: Class<'a>,
    pub name: &'a str,
    pub descriptor: MethodDescriptor<'a>,
}

#[derive(Debug, Clone)]
pub struct Fieldref<'a> {
    pub class: Class<'a>,
    pub name: &'a str,
    pub descriptor: FieldDesc<'a>,
}

#[derive(Debug, Clone)]
pub struct NameAndType {
    pub name: Utf8Index,
    pub descriptor: Utf8Index,
}

#[derive(Debug, Clone)]
pub struct MethodNameAndType<'a> {
    pub name: &'a str,
    pub descriptor: MethodDescriptor<'a>,
}

#[derive(Debug, Clone)]
pub struct FieldNameAndType<'a> {
    pub name: &'a str,
    pub descriptor: FieldDesc<'a>,
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

#[derive(Debug, Clone)]
pub struct InvokeDynamic {
    pub bootstrap_method_attr_index: u16,
    pub name_and_type: NameAndTypeIndex,
}
