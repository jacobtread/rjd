use classfile::{
    attributes::Code,
    class::Method,
    constant_pool::{
        ClassItem, ConstantItem, ConstantPool, MethodNameAndType, Methodref, PoolIndex,
    },
    inst::{ArrayType, Instruction, LookupSwitchData},
    types::{FieldDesc, MethodDescriptor},
};
use thiserror::Error;

#[derive(Debug)]
pub enum AST<'a> {
    Value(Value<'a>),
    /// Operation on two different AST items
    Operation(Operation<'a>),
    /// Negated value -1
    Negated(Box<AST<'a>>),
    /// Conditial statement
    Condition(JumpCondition<'a>),
    /// Integer switch
    IntegerSwitch(IntegerSwitch<'a>),
    /// Return with optional value
    Return(Option<Box<AST<'a>>>),
    /// Variable
    LocalVariable(u16, LocalVariableType),

    MethodCall(MethodCall<'a>),

    StaticMethodCall(StaticMethodCall<'a>),

    Other(Instruction),
}

#[derive(Debug)]

pub struct MethodCall<'a> {
    pub method_ref: Methodref<'a>,
    pub reference: Box<AST<'a>>,
    pub args: Vec<AST<'a>>,
}

#[derive(Debug)]

pub struct StaticMethodCall<'a> {
    pub method_ref: Methodref<'a>,
    pub args: Vec<AST<'a>>,
}

#[derive(Debug)]
pub enum LocalVariableType {
    Int,
    Long,
    Float,
    Double,
    Reference,
}

#[derive(Debug)]

pub struct IntegerSwitch<'a> {
    pub key: Box<AST<'a>>,
    pub data: LookupSwitchData,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum ConditionType {
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
}

#[derive(Debug)]
pub struct Operation<'a> {
    pub left: Box<AST<'a>>,
    pub ty: OperationType,
    pub right: Box<AST<'a>>,
}

#[derive(Debug)]
pub enum Value<'a> {
    Null,
    String(&'a str),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    Short(i16),
}

#[derive(Debug)]
pub struct PrimitiveArray<'a> {
    pub count: Box<AST<'a>>,
    pub ty: ArrayType,
}

#[derive(Debug, Error)]
pub enum InstrError {
    #[error("Missing constant at index {0}")]
    MissingConstant(PoolIndex),
    #[error("Unexpected constant pool type at index {0}")]
    UnexpectedConstantType(PoolIndex),
    #[error(transparent)]
    Stack(#[from] StackError),
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

#[derive(Debug)]
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
}

type StackResult<T> = Result<T, StackError>;

impl<'a> Stack<'a> {
    fn push(&mut self, value: AST<'a>) {
        self.inner.push(value);
    }

    fn pop(&mut self) -> StackResult<AST<'a>> {
        self.inner.pop().ok_or(StackError::Empty)
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

pub fn generate_ast<'a, 'b: 'a>(
    code: &Code,
    pool: &'b ConstantPool<'a>,
) -> Result<Vec<AST<'a>>, InstrError> {
    use classfile::inst::Instruction::*;

    let mut stack: Stack<'a> = Stack::default();
    let mut ast: Vec<AST> = Vec::new();

    for (_pos, instr) in &code.code {
        match instr {
            // Swap the top two stack items
            Swap => {
                stack.swap()?;
            }

            // Normal method calls
            InvokeSpecial(index) | InvokeInterface(index) | InvokeVirtual(index) => {
                // TODO: Handle these errors
                let class_item = pool.get_methodref(*index).unwrap();
                let methodref = pool.get_methodref_actual(class_item).unwrap();

                let mut args =
                    Vec::with_capacity(methodref.name_and_type.descriptor.parameters.len());
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

                if let FieldDesc::Void = &call.method_ref.name_and_type.descriptor.return_type {
                    ast.push(AST::MethodCall(call))
                } else {
                    stack.push(AST::MethodCall(call))
                }
            }

            // Static method calls
            InvokeStatic(index) => {
                // TODO: Handle these errors
                let class_item = pool.get_methodref(*index).unwrap();
                let methodref = pool.get_methodref_actual(class_item).unwrap();

                let mut args =
                    Vec::with_capacity(methodref.name_and_type.descriptor.parameters.len());
                for _ in 0..args.len() {
                    args.push(stack.pop()?);
                }
                args.reverse();

                let call = StaticMethodCall {
                    method_ref: methodref,
                    args,
                };
                if let FieldDesc::Void = &call.method_ref.name_and_type.descriptor.return_type {
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

            // Negated value !value
            INeg | FNeg | DNeg | LNeg => {
                let value = stack.pop_boxed()?;
                stack.push(AST::Negated(value))
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

            // return value; Return with value
            AReturn | IReturn | FReturn | DReturn | LReturn => {
                let value = stack.pop_boxed()?;
                ast.push(AST::Return(Some(value)))
            }

            // return; Void return
            Return => ast.push(AST::Return(None)),

            value => ast.push(AST::Other(value.clone())),
        }
    }

    Ok(ast)
}
