use core::panic;

use classfile::{
    attributes::Code,
    constant_pool::{ConstantItem, ConstantPool, Fieldref, InvokeDynamic, Methodref, PoolIndex},
    inst::{ArrayType, Instruction, LookupSwitchData, TableSwitchData},
    types::{Class, FieldDesc},
};
use thiserror::Error;

#[derive(Debug, Clone)]
pub enum AST<'a> {
    /// Integer switch
    IntegerSwitch(IntegerSwitch<'a>),
    TableSwitch(TableSwitchImpl<'a>),

    Throw(Value<'a>),
    Jsr(u16),
    Ret(u16),
    Other(Instruction),
    InvokeDynamic(InvokeDynamic),

    /// Increment local var at index by value (index, value)
    Increment(u16, i16),
    Value(Value<'a>),
    /// Operation on two different AST items
    Operation(Operation<'a>),
    ArrayStore(ArrayStore<'a>),
    ArrayLoad(ArrayLoad<'a>),
    /// Conditial statement
    Condition(JumpCondition<'a>),
    /// Return with optional value
    Return(Option<Box<AST<'a>>>),
    /// Variable
    LocalVarriable(u16, LocalVariableType),
    // Store a value in a local variable
    SetLocalVariable(u16, Box<AST<'a>>),
    MethodCall(MethodCall<'a>),
    StaticMethodCall(StaticMethodCall<'a>),
    PutField(PutField<'a>),
    GetField(GetField<'a>),
    PutStatic(PutStatic<'a>),
    GetStatic(GetStatic<'a>),
}

#[derive(Debug, Clone)]

pub struct ArrayStore<'a> {
    pub reference: Box<AST<'a>>,
    pub index: Box<AST<'a>>,
    pub value: Box<AST<'a>>,
}

#[derive(Debug, Clone)]
pub struct ArrayLoad<'a> {
    pub reference: Box<AST<'a>>,
    pub index: Box<AST<'a>>,
}

#[derive(Debug, Clone)]
pub struct PutField<'a> {
    pub field: Fieldref<'a>,
    pub value: Box<AST<'a>>,
    pub reference: Box<AST<'a>>,
}

#[derive(Debug, Clone)]
pub struct GetField<'a> {
    pub field: Fieldref<'a>,
    pub reference: Box<AST<'a>>,
}

#[derive(Debug, Clone)]
pub struct PutStatic<'a> {
    pub field: Fieldref<'a>,
    pub value: Box<AST<'a>>,
}

#[derive(Debug, Clone)]
pub struct GetStatic<'a> {
    pub field: Fieldref<'a>,
}

#[derive(Debug, Clone)]

pub struct MethodCall<'a> {
    pub method_ref: Methodref<'a>,
    pub reference: Box<AST<'a>>,
    pub args: Vec<AST<'a>>,
}

#[derive(Debug, Clone)]

pub struct StaticMethodCall<'a> {
    pub method_ref: Methodref<'a>,
    pub args: Vec<AST<'a>>,
}

#[derive(Debug, Clone)]
pub enum LocalVariableType {
    Int,
    Long,
    Float,
    Double,
    Reference,
}

#[derive(Debug, Clone)]

pub struct IntegerSwitch<'a> {
    pub key: Box<AST<'a>>,
    pub data: LookupSwitchData,
}

#[derive(Debug, Clone)]

pub struct TableSwitchImpl<'a> {
    pub key: Box<AST<'a>>,
    pub data: TableSwitchData,
}

#[derive(Debug, Clone)]
pub struct JumpCondition<'a> {
    /// Left hand side of the condition
    pub left: Box<AST<'a>>,
    /// Right hand side of the condition
    pub right: Box<AST<'a>>,
    /// The actual condition
    pub ty: ConditionType,
    /// The position to jump to
    pub jump_index: u16,
}

#[derive(Debug, Clone)]
pub enum ConditionType {
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
}

#[derive(Debug, Clone)]
pub struct Operation<'a> {
    pub left: Box<AST<'a>>,
    pub ty: OperationType,
    pub right: Box<AST<'a>>,
}

#[derive(Debug, Clone)]
pub enum OperationType {
    Muliply,
    Divide,
    Subtract,
    Add,
    CompareGreater,
    CompareLess,
    SignedCompare,
    Xor,
    BitwiseAnd,
    BitwiseOr,
    BitwiseShl,
    BitwiseShr,
    LogicalShr,
    Remainder,
    InstanceOf,
}

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Null,
    String(&'a str),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    Short(i16),
    Char(char),
    Byte(u8),

    Objectref(Class<'a>),
    Arrayref(Arrayref<'a>),
}

#[derive(Debug, Clone)]
pub enum Arrayref<'a> {
    Primitive(PrimitiveArray<'a>),
    Reference(ReferenceArray<'a>),
    MultiReference(MultiReferenceArray<'a>),
}

impl<'a> Value<'a> {
    fn cast(&self, target: FieldDesc<'a>) -> Result<Value<'a>, InstrError> {
        Ok(match (self, target) {
            // float/double/int => long
            (Self::Float(value), FieldDesc::Long) => Value::Long(*value as i64),
            (Self::Double(value), FieldDesc::Long) => Value::Long(*value as i64),
            (Self::Integer(value), FieldDesc::Long) => Value::Long(*value as i64),

            // float/int/long => double
            (Self::Float(value), FieldDesc::Double) => Value::Double(*value as f64),
            (Self::Integer(value), FieldDesc::Double) => Value::Double(*value as f64),
            (Self::Long(value), FieldDesc::Double) => Value::Double(*value as f64),

            // float/double/long => int
            (Self::Float(value), FieldDesc::Int) => Value::Integer(*value as i32),
            (Self::Double(value), FieldDesc::Int) => Value::Integer(*value as i32),
            (Self::Long(value), FieldDesc::Int) => Value::Integer(*value as i32),

            // int/double/long => float
            (Self::Integer(value), FieldDesc::Float) => Value::Float(*value as f32),
            (Self::Double(value), FieldDesc::Float) => Value::Float(*value as f32),
            (Self::Long(value), FieldDesc::Float) => Value::Float(*value as f32),

            // int => byte
            (Self::Integer(value), FieldDesc::Byte) => Value::Byte(*value as u8),
            // int => short
            (Self::Integer(value), FieldDesc::Short) => Value::Short(*value as i16),
            // int => char
            (Self::Integer(value), FieldDesc::Char) => Value::Char((*value as u8) as char),

            _ => return Err(InstrError::ImpossibleCast),
        })
    }
}

#[derive(Debug, Clone)]
pub struct PrimitiveArray<'a> {
    pub count: Box<AST<'a>>,
    pub ty: ArrayType,
}

#[derive(Debug, Clone)]
pub struct ReferenceArray<'a> {
    pub count: Box<AST<'a>>,
    pub ty: Class<'a>,
}
#[derive(Debug, Clone)]
pub struct MultiReferenceArray<'a> {
    pub counts: Vec<AST<'a>>,
    pub ty: Class<'a>,
}

#[derive(Debug, Error)]
pub enum InstrError {
    #[error("Missing constant at index {0}")]
    MissingConstant(PoolIndex),
    #[error("Unexpected constant pool type at index {0}")]
    UnexpectedConstantType(PoolIndex),
    #[error(transparent)]
    Stack(#[from] StackError),

    #[error("Cannot cast between value types")]
    ImpossibleCast,
}

impl<'a> Value<'a> {
    pub fn try_from_pool(
        index: PoolIndex,
        pool: &ConstantPool<'a>,
    ) -> Result<Value<'a>, InstrError> {
        let value = pool.get(index).ok_or(InstrError::MissingConstant(index))?;
        Ok(match value {
            ConstantItem::Integer(value) => Value::Integer(*value),
            ConstantItem::Float(value) => Value::Float(*value),
            ConstantItem::Long(value) => Value::Long(*value),
            ConstantItem::Double(value) => Value::Double(*value),
            ConstantItem::String(value) => {
                let value = pool
                    .get_utf8(*value)
                    .ok_or(InstrError::UnexpectedConstantType(index))?;
                Value::String(value)
            }
            _ => return Err(InstrError::UnexpectedConstantType(index)),
        })
    }
}

#[derive(Default)]
pub struct Stack<'a> {
    inner: Vec<AST<'a>>,
}

#[derive(Debug, Error)]
pub enum StackError {
    /// Stack was empty
    #[error("Stack was empty")]
    Empty,
    /// Stack didn't have enough items to pop
    #[error("Not enough items to pop from stack")]
    NotEnough {
        /// The length required
        required: usize,
        /// The length remaining
        length: usize,
    },

    #[error("Expected value on stack")]
    ExpectedValue,
}

type StackResult<T> = Result<T, StackError>;

pub enum StackItem {}

impl<'a> Stack<'a> {
    fn push(&mut self, value: AST<'a>) {
        self.inner.push(value);
    }

    fn pop(&mut self) -> StackResult<AST<'a>> {
        self.inner.pop().ok_or(StackError::Empty)
    }

    fn clone_last(&mut self) -> StackResult<AST<'a>> {
        self.inner.last().cloned().ok_or(StackError::Empty)
    }

    fn dup(&mut self) -> StackResult<()> {
        let value = self.clone_last()?;
        self.push(value);
        Ok(())
    }

    fn dup_x1(&mut self) -> StackResult<()> {
        let value = self.clone_last()?;
        // TODO: Bounds checking
        self.inner.insert(self.inner.len() - 2, value);
        Ok(())
    }

    fn dup_x2(&mut self) -> StackResult<()> {
        let value = self.clone_last()?;
        // TODO: Bounds checking
        self.inner.insert(self.inner.len() - 3, value);
        Ok(())
    }

    fn dup_2(&mut self) -> StackResult<()> {
        let length = self.inner.len();
        if length < 2 {
            return Err(StackError::NotEnough {
                required: 2,
                length,
            });
        }

        // TODO: Bounds checking
        let a = self.inner[length - 1].clone();
        let b = self.inner[length - 2].clone();

        self.push(a);
        self.push(b);
        Ok(())
    }

    fn dup_2x1(&mut self) -> StackResult<()> {
        let length = self.inner.len();
        if length < 2 {
            return Err(StackError::NotEnough {
                required: 2,
                length,
            });
        }

        let a = self.inner[length - 1].clone();
        let b = self.inner[length - 2].clone();

        // TODO: Bounds checking
        self.inner.insert(length - 2, a);
        self.inner.insert(length - 3, b);

        Ok(())
    }

    fn dup_2x2(&mut self) -> StackResult<()> {
        let length = self.inner.len();
        if length < 2 {
            return Err(StackError::NotEnough {
                required: 2,
                length,
            });
        }

        let a = self.inner[length - 1].clone();
        let b = self.inner[length - 2].clone();

        self.inner.insert(length - 3, a);
        self.inner.insert(length - 4, b);

        Ok(())
    }

    fn pop_value(&mut self) -> StackResult<Value<'a>> {
        match self.inner.pop() {
            Some(AST::Value(value)) => Ok(value),
            Some(_) => Err(StackError::ExpectedValue),
            None => Err(StackError::Empty),
        }
    }

    fn try_cast_value(&mut self, target: FieldDesc<'a>) -> StackResult<()> {
        let value = self.pop_value()?;
        // TODO: handle failed casts
        let casted = value.cast(target).unwrap();
        self.push(AST::Value(casted));
        Ok(())
    }

    fn pop2(&mut self) -> StackResult<()> {
        let top = self.pop()?;
        if !matches!(top, AST::Value(Value::Double(_) | Value::Long(_))) {
            self.pop()?;
        }

        Ok(())
    }

    fn pop_boxed(&mut self) -> StackResult<Box<AST<'a>>> {
        self.pop().map(Box::new)
    }

    fn swap(&mut self) -> StackResult<()> {
        let length = self.inner.len();
        if length < 2 {
            return Err(StackError::NotEnough {
                required: 2,
                length,
            });
        }
        // Swaps the top two items on the stack
        self.inner.swap(length - 1, length - 2);
        Ok(())
    }
}

pub fn pinstr<'a, 'b: 'a>(
    instr: &'b Instruction,
    pool: &'b ConstantPool<'a>,
    stack: &mut Stack<'a>,
    ast: &mut Vec<AST<'a>>,
) -> Result<(), InstrError> {
    use classfile::inst::Instruction::*;

    match instr {
        // Swap the top two stack items
        Swap => {
            stack.swap()?;
        }

        New(index) => {
            // TODO: Handle errors
            let class = Class::try_parse(pool.get_class_name(*index).unwrap()).unwrap();
            stack.push(AST::Value(Value::Objectref(class)))
        }

        NewArray(array_type) => {
            let count = stack.pop_boxed()?;
            stack.push(AST::Value(Value::Arrayref(Arrayref::Primitive(
                PrimitiveArray {
                    count,
                    ty: *array_type,
                },
            ))))
        }

        ANewArray(index) => {
            let count = stack.pop_boxed()?;
            let class = Class::try_parse(pool.get_class_name(*index).unwrap()).unwrap();
            stack.push(AST::Value(Value::Arrayref(Arrayref::Reference(
                ReferenceArray { count, ty: class },
            ))))
        }

        MultiANewArray { index, dimensions } => {
            let class = Class::try_parse(pool.get_class_name(*index).unwrap()).unwrap();
            let mut counts = Vec::with_capacity(*dimensions as usize);
            for _ in 0..*dimensions {
                counts.push(stack.pop()?);
            }
            stack.push(AST::Value(Value::Arrayref(Arrayref::MultiReference(
                MultiReferenceArray { counts, ty: class },
            ))))
        }

        // Normal method calls
        InvokeSpecial(index) | InvokeInterface(index) | InvokeVirtual(index) => {
            // TODO: Handle these errors
            let class_item = pool.get_methodref(*index).unwrap();
            let methodref = pool.get_methodref_actual(class_item).unwrap();

            let mut args = Vec::with_capacity(methodref.descriptor.parameters.len());
            for _ in 0..args.len() {
                args.push(stack.pop()?);
            }
            args.reverse();
            let reference = stack.pop_boxed()?;

            let call = MethodCall {
                method_ref: methodref,
                reference,
                args,
            };

            if !matches!(&call.method_ref.descriptor.return_type, FieldDesc::Void) {
                stack.push(AST::MethodCall(call.clone()))
            }

            ast.push(AST::MethodCall(call))
        }

        // Static method calls
        InvokeStatic(index) => {
            // TODO: Handle these errors
            let class_item = pool.get_methodref(*index).unwrap();
            let methodref = pool.get_methodref_actual(class_item).unwrap();

            let mut args = Vec::with_capacity(methodref.descriptor.parameters.len());
            for _ in 0..args.len() {
                args.push(stack.pop()?);
            }
            args.reverse();

            let call = StaticMethodCall {
                method_ref: methodref,
                args,
            };
            if let FieldDesc::Void = &call.method_ref.descriptor.return_type {
                ast.push(AST::StaticMethodCall(call))
            } else {
                stack.push(AST::StaticMethodCall(call))
            }
        }

        // Local variable loads
        ILoad(index) => stack.push(AST::LocalVariable(*index, LocalVariableType::Int)),
        LLoad(index) => stack.push(AST::LocalVariable(*index, LocalVariableType::Long)),
        FLoad(index) => stack.push(AST::LocalVariable(*index, LocalVariableType::Float)),
        DLoad(index) => stack.push(AST::LocalVariable(*index, LocalVariableType::Double)),
        ALoad(index) => stack.push(AST::LocalVariable(*index, LocalVariableType::Reference)),
        IStore(index) | LStore(index) | FStore(index) | DStore(index) | AStore(index) => {
            let value = stack.pop_boxed()?;
            ast.push(AST::SetLocalVariable(*index, value))
        }

        PutField(index) => {
            // TODO: Handle these errors
            let class_item = pool.get_fieldref(*index).unwrap();
            let field = pool.get_fieldref_actual(class_item).unwrap();

            let value = stack.pop_boxed()?;
            let reference = stack.pop_boxed()?;

            ast.push(AST::PutField(super::ast::PutField {
                field,
                value,
                reference,
            }))
        }

        GetField(index) => {
            // TODO: Handle these errors
            let class_item = pool.get_fieldref(*index).unwrap();
            let field = pool.get_fieldref_actual(class_item).unwrap();
            let reference = stack.pop_boxed()?;

            stack.push(AST::GetField(super::ast::GetField { field, reference }))
        }

        PutStatic(index) => {
            // TODO: Handle these errors
            let class_item = pool.get_fieldref(*index).unwrap();
            let field = pool.get_fieldref_actual(class_item).unwrap();

            let value = stack.pop_boxed()?;

            ast.push(AST::PutStatic(super::ast::PutStatic { field, value }))
        }

        GetStatic(index) => {
            // TODO: Handle these errors
            let class_item = pool.get_fieldref(*index).unwrap();
            let field = pool.get_fieldref_actual(class_item).unwrap();

            stack.push(AST::GetStatic(super::ast::GetStatic { field }))
        }

        // Push constant onto the stack from the constant pool
        LoadConst(index) => {
            let value = Value::try_from_pool(*index, pool)?;
            stack.push(AST::Value(value))
        }

        // Pushing constants
        IConst(value) => stack.push(AST::Value(Value::Integer(*value))),
        DConst(value) => stack.push(AST::Value(Value::Double(*value))),
        FConst(value) => stack.push(AST::Value(Value::Float(*value))),
        LConst(value) => stack.push(AST::Value(Value::Long(*value))),
        AConstNull => stack.push(AST::Value(Value::Null)),

        BIPush(value) => stack.push(AST::Value(Value::Integer(*value as i32))),
        SIPush(value) => stack.push(AST::Value(Value::Short(*value))),

        MonitorEnter | MonitorExit | Pop => {
            stack.pop()?;
        }
        Pop2 => stack.pop2()?,

        // * Multiply operation
        IMul | FMul | DMul | LMul => {
            let left = stack.pop_boxed()?;
            let right = stack.pop_boxed()?;
            stack.push(AST::Operation(Operation {
                left,
                ty: OperationType::Muliply,
                right,
            }))
        }

        // / Divide operation
        IDiv | FDiv | DDiv | LDiv => {
            let left = stack.pop_boxed()?;
            let right = stack.pop_boxed()?;
            stack.push(AST::Operation(Operation {
                left,
                ty: OperationType::Divide,
                right,
            }))
        }

        // + Add operation
        IAdd | FAdd | DAdd | LAdd => {
            let left = stack.pop_boxed()?;
            let right = stack.pop_boxed()?;
            stack.push(AST::Operation(Operation {
                left,
                ty: OperationType::Add,
                right,
            }))
        }

        // - Subtract operation
        ISub | FSub | DSub | LSub => {
            let left = stack.pop_boxed()?;
            let right = stack.pop_boxed()?;
            stack.push(AST::Operation(Operation {
                left,
                ty: OperationType::Subtract,
                right,
            }))
        }

        // % Remainder operation
        IRem | FRem | DRem | LRem => {
            let left = stack.pop_boxed()?;
            let right = stack.pop_boxed()?;
            stack.push(AST::Operation(Operation {
                left,
                ty: OperationType::Remainder,
                right,
            }))
        }

        // < Less than comparison (float, double)
        FCmpL | DCmpL => {
            let left = stack.pop_boxed()?;
            let right = stack.pop_boxed()?;
            stack.push(AST::Operation(Operation {
                left,
                ty: OperationType::CompareLess,
                right,
            }))
        }

        // > Less than comparison (float, double)
        FCmpG | DCmpG => {
            let left = stack.pop_boxed()?;
            let right = stack.pop_boxed()?;
            stack.push(AST::Operation(Operation {
                left,
                ty: OperationType::CompareGreater,
                right,
            }))
        }

        // Signed compare on two longs
        LCmp => {
            let left = stack.pop_boxed()?;
            let right = stack.pop_boxed()?;
            stack.push(AST::Operation(Operation {
                left,
                ty: OperationType::SignedCompare,
                right,
            }))
        }

        // & Bitwise AND operation
        IAnd | LAnd => {
            let left = stack.pop_boxed()?;
            let right = stack.pop_boxed()?;
            stack.push(AST::Operation(Operation {
                left,
                ty: OperationType::BitwiseAnd,
                right,
            }))
        }

        // | Bitwise OR operation
        IOr | LOr => {
            let left = stack.pop_boxed()?;
            let right = stack.pop_boxed()?;
            stack.push(AST::Operation(Operation {
                left,
                ty: OperationType::BitwiseOr,
                right,
            }))
        }

        // ^ XOR operation
        IXOr | LXOr => {
            let left = stack.pop_boxed()?;
            let right = stack.pop_boxed()?;
            stack.push(AST::Operation(Operation {
                left,
                ty: OperationType::Xor,
                right,
            }))
        }

        // << Bitwise shift left operation
        IShL | LShL => {
            let left = stack.pop_boxed()?;
            let right = stack.pop_boxed()?;
            stack.push(AST::Operation(Operation {
                left,
                ty: OperationType::BitwiseShl,
                right,
            }))
        }

        // >> Bitwise shift right operation
        IShR | LShR => {
            let left = stack.pop_boxed()?;
            let right = stack.pop_boxed()?;
            stack.push(AST::Operation(Operation {
                left,
                ty: OperationType::BitwiseShr,
                right,
            }))
        }

        // >>> Logical shift right operation
        IUShR | LUShR => {
            let left = stack.pop_boxed()?;
            let right = stack.pop_boxed()?;
            stack.push(AST::Operation(Operation {
                left,
                ty: OperationType::LogicalShr,
                right,
            }))
        }

        // Negated value -value
        INeg | FNeg | DNeg | LNeg => {
            let value = stack.pop_value()?;
            let new_value: Value = match value {
                Value::Integer(value) => Value::Integer(-value),
                Value::Float(value) => Value::Float(-value),
                Value::Double(value) => Value::Double(-value),
                Value::Long(value) => Value::Long(-value),
                _ => panic!("Unexpected value type TODO: HAndle"),
            };

            // TODO: USE NEGATED STACK ITEM

            stack.push(AST::Value(new_value))
        }

        // a == b int/ref compare equal with jump
        IfICmpEq(index) | IfACmpEq(index) => {
            let left = stack.pop_boxed()?;
            let right = stack.pop_boxed()?;

            ast.push(AST::Condition(JumpCondition {
                left,
                right,
                ty: ConditionType::Equal,
                jump_index: *index,
            }))
        }

        // a != b int/ref compare not equal with jump
        IfICmpNe(index) | IfACmpNe(index) => {
            let left = stack.pop_boxed()?;
            let right = stack.pop_boxed()?;

            ast.push(AST::Condition(JumpCondition {
                left,
                right,
                ty: ConditionType::NotEqual,
                jump_index: *index,
            }))
        }

        // a > b int compare not equal with jump
        IfICmpGt(index) => {
            let left = stack.pop_boxed()?;
            let right = stack.pop_boxed()?;

            ast.push(AST::Condition(JumpCondition {
                left,
                right,
                ty: ConditionType::GreaterThan,
                jump_index: *index,
            }))
        }

        // a >= b int compare not equal with jump
        IfICmpGe(index) => {
            let left = stack.pop_boxed()?;
            let right = stack.pop_boxed()?;

            ast.push(AST::Condition(JumpCondition {
                left,
                right,
                ty: ConditionType::GreaterThanOrEqual,
                jump_index: *index,
            }))
        }

        // a < b int compare not equal with jump
        IfICmpLt(index) => {
            let left = stack.pop_boxed()?;
            let right = stack.pop_boxed()?;

            ast.push(AST::Condition(JumpCondition {
                left,
                right,
                ty: ConditionType::LessThan,
                jump_index: *index,
            }))
        }

        // a <= b int compare not equal with jump
        IfICmpLe(index) => {
            let left = stack.pop_boxed()?;
            let right = stack.pop_boxed()?;

            ast.push(AST::Condition(JumpCondition {
                left,
                right,
                ty: ConditionType::LessThanOrEqual,
                jump_index: *index,
            }))
        }

        // value != null non null comparison
        IfNonNull(index) => {
            let left = stack.pop_boxed()?;
            let right = Box::new(AST::Value(Value::Null));

            // left MUST be a reference or null constant

            ast.push(AST::Condition(JumpCondition {
                left,
                right,
                ty: ConditionType::NotEqual,
                jump_index: *index,
            }))
        }

        // value == null null comparison
        IfNull(index) => {
            let left = stack.pop_boxed()?;
            let right = Box::new(AST::Value(Value::Null));

            // left MUST be a reference or null constant

            ast.push(AST::Condition(JumpCondition {
                left,
                right,
                ty: ConditionType::Equal,
                jump_index: *index,
            }))
        }

        // Integer lookup switch case
        LookupSwitch(data) => {
            let key = stack.pop_boxed()?;
            // key MUST be a int

            ast.push(AST::IntegerSwitch(IntegerSwitch {
                key,
                data: data.clone(),
            }))
        }

        TableSwitch(data) => {
            let key = stack.pop_boxed()?;
            // key MUST be a int

            ast.push(AST::TableSwitch(TableSwitchImpl {
                key,
                data: data.clone(),
            }))
        }

        // return value; Return with value
        AReturn | IReturn | FReturn | DReturn | LReturn => {
            let value = stack.pop_boxed()?;
            ast.push(AST::Return(Some(value)))
        }

        // return; Void return
        Return => ast.push(AST::Return(None)),

        IInc { index, value } => ast.push(AST::Increment(*index, *value)),

        // (float/double/int) as long cast
        F2l | D2l | I2l => stack.try_cast_value(FieldDesc::Long)?,

        // (float/int/long) as double cast
        F2d | I2d | L2d => stack.try_cast_value(FieldDesc::Double)?,

        // (float/double/long) as int cast
        F2i | D2i | L2i => stack.try_cast_value(FieldDesc::Int)?,

        // (int/double/long) as float cast
        I2f | D2f | L2f => stack.try_cast_value(FieldDesc::Float)?,

        // int as byte cast
        I2b => stack.try_cast_value(FieldDesc::Byte)?,
        // int as short cast
        I2s => stack.try_cast_value(FieldDesc::Short)?,
        // int as char cast
        I2c => stack.try_cast_value(FieldDesc::Char)?,

        IAStore | LAStore | DAStore | CAStore | BAStore | AAStore | SAStore | FAStore => {
            let reference = stack.pop_boxed()?;
            let index = stack.pop_boxed()?;
            let value = stack.pop_boxed()?;
            ast.push(AST::ArrayStore(ArrayStore {
                reference,
                index,
                value,
            }))
        }

        IALoad | LALoad | DALoad | CALoad | BALoad | AALoad | SALoad | FALoad => {
            let reference = stack.pop_boxed()?;
            let index = stack.pop_boxed()?;
            stack.push(AST::ArrayLoad(ArrayLoad { reference, index }))
        }

        Dup => stack.dup()?,
        DupX1 => stack.dup_x1()?,
        DupX2 => stack.dup_x2()?,
        Dup2 => stack.dup_2()?,
        Dup2X1 => stack.dup_2x1()?,
        Dup2X2 => stack.dup_2x2()?,

        // Ignored stack behavior
        CheckCast(_) => {}
        ArrayLength => {
            let reference = stack.pop_boxed();
            stack.push(AST::Value(Value::Integer(0)))
        }
        AThrow => {
            // Should be the exception
            let thrown = stack.pop_value()?;
            stack.push(AST::Value(thrown.clone()));
            ast.push(AST::Throw(thrown))
        }
        InstanceOf(index) => {
            let class = Class::try_parse(pool.get_class_name(*index).unwrap()).unwrap();
            let value = stack.pop_value()?;

            stack.push(AST::Operation(Operation {
                left: Box::new(AST::Value(value)),
                ty: OperationType::InstanceOf,
                right: Box::new(AST::Value(Value::Objectref(class))),
            }));
        }

        IfEq(index) => {
            let left = stack.pop_boxed()?;
            let right = Box::new(AST::Value(Value::Byte(0)));
            ast.push(AST::Condition(JumpCondition {
                left,
                right,
                ty: ConditionType::Equal,
                jump_index: *index,
            }))
        }
        IfNe(index) => {
            let left = stack.pop_boxed()?;
            let right = Box::new(AST::Value(Value::Byte(0)));
            ast.push(AST::Condition(JumpCondition {
                left,
                right,
                ty: ConditionType::NotEqual,
                jump_index: *index,
            }))
        }
        IfLt(index) => {
            let left = stack.pop_boxed()?;
            let right = Box::new(AST::Value(Value::Byte(0)));
            ast.push(AST::Condition(JumpCondition {
                left,
                right,
                ty: ConditionType::LessThan,
                jump_index: *index,
            }))
        }
        IfGe(index) => {
            let left = stack.pop_boxed()?;
            let right = Box::new(AST::Value(Value::Byte(0)));
            ast.push(AST::Condition(JumpCondition {
                left,
                right,
                ty: ConditionType::GreaterThanOrEqual,
                jump_index: *index,
            }))
        }
        IfGt(index) => {
            let left = stack.pop_boxed()?;
            let right = Box::new(AST::Value(Value::Byte(0)));
            ast.push(AST::Condition(JumpCondition {
                left,
                right,
                ty: ConditionType::GreaterThan,
                jump_index: *index,
            }))
        }
        IfLe(index) => {
            let left = stack.pop_boxed()?;
            let right = Box::new(AST::Value(Value::Byte(0)));
            ast.push(AST::Condition(JumpCondition {
                left,
                right,
                ty: ConditionType::LessThanOrEqual,
                jump_index: *index,
            }))
        }

        InvokeDynamic(index) => {
            // TODO: Error handling
            let dynamic = pool.get_invokedynamic(*index).unwrap();
            ast.push(AST::InvokeDynamic(dynamic))
        }

        // Ignored instructions
        Goto(_) | Nop => {}
        // TODO: Properly implement jsr
        JSr(index) => stack.push(AST::Jsr(*index)),
        // TODO: Properly implement Ret
        Ret(index) => stack.push(AST::Jsr(*index)),
    }

    Ok(())
}
