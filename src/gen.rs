use std::collections::HashMap;

use classfile::{
    attributes::{BorrowedInstrSet, InstructionSet},
    constant_pool::{ConstantItem, ConstantPool, Fieldref, InvokeDynamic, Methodref, PoolIndex},
    inst::{ArrayType, Instruction, LookupSwitchData, TableSwitchData},
    types::{Class, FieldDesc},
};
use thiserror::Error;

/// Block of instructions
#[derive(Debug)]
pub struct Block<'set> {
    instructions: BorrowedInstrSet<'set>,
    branches: Vec<usize>,
}

impl<'set> Block<'set> {
    pub fn decompile<'a, 'b: 'a>(
        &'b self,
        pool: &'b ConstantPool<'a>,
    ) -> Result<Vec<AST<'a>>, ProcessError> {
        let mut stack: Stack<'a> = Stack::default();
        let mut ast: Vec<AST<'a>> = Vec::new();

        let mut iter = self.instructions.inner.iter();

        for (_pos, instr) in iter.by_ref() {
            if let Err(err) = process(instr, pool, &mut stack, &mut ast) {
                eprintln!("ERR: {:?}", err);
                ast.push(AST::Instruction(instr.clone()));
                break;
            }
        }

        for (_pos, instr) in iter {
            ast.push(AST::Instruction(instr.clone()))
        }

        Ok(ast)
    }
}

pub fn create_blocks(input: &InstructionSet) -> HashMap<usize, Block> {
    input
        .split_jumps()
        .into_iter()
        .map(|set| {
            let first_pos = set.inner[0].0;

            let (last_pos, last_instr) = set.inner.last().unwrap();

            let next = input
                .inner
                .iter()
                .find_map(|(pos, _)| if pos > last_pos { Some(*pos) } else { None });

            use classfile::inst::Instruction::*;
            let mut branches = Vec::new();

            match last_instr {
                IfNe(branch) | IfEq(branch) | IfLe(branch) | IfGe(branch) | IfGt(branch)
                | IfLt(branch) | IfICmpEq(branch) | IfICmpNe(branch) | IfICmpGt(branch)
                | IfICmpGe(branch) | IfICmpLt(branch) | IfICmpLe(branch) => {
                    let next_pos = next.unwrap();
                    branches.push(*branch as usize);
                    branches.push(next_pos);
                }
                Goto(branch) => {
                    branches.push(*branch as usize);
                }
                Return | AReturn | IReturn | LReturn | DReturn | FReturn => {}
                _ => {
                    let next_pos = next.unwrap();
                    branches.push(next_pos);
                }
            }

            (
                first_pos,
                Block {
                    instructions: set,
                    branches,
                },
            )
        })
        .collect()
}

/// Represents the different items that can be placed
/// onto the stack
#[derive(Debug, Clone)]
pub enum StackItem<'a> {
    /// Represents an actual value
    Value(Value<'a>),
    /// Represents the casting of a literal type
    LiteralCast {
        /// The value being cast
        value: Box<StackItem<'a>>,
        /// The type that it was cast to
        cast_to: FieldDesc<'a>,
    },

    /// Represents a checked cast to a specific object type
    CheckedCast {
        value: Box<StackItem<'a>>,
        cast_to: Class<'a>,
    },

    /// Represents an operation between two stack items
    Operation {
        /// The right hand side of the operation
        right: Box<StackItem<'a>>,
        /// The type of operation
        ty: OperationType,
        /// The left hand side of the operation
        left: Box<StackItem<'a>>,
    },
    Call(Call<'a>),
    CallStatic(CallStatic<'a>),
    /// Represents getting a local variable on the stack
    GetLocal {
        /// The index of the local variable
        index: u16,
        /// The type of the local variable
        ty: LocalVariableType,
    },
    /// Represents getting a field value from an object
    GetField {
        /// The reference to the field
        field: Fieldref<'a>,
        /// Reference to the object the field is on
        reference: Box<StackItem<'a>>,
    },
    /// Represents getting a static field value
    GetFieldStatic {
        /// The reference to the field
        field: Fieldref<'a>,
    },
    /// Represents access of the .length field on an array
    ArrayLength {
        /// Reference to the array
        reference: Box<StackItem<'a>>,
    },
    /// Represents loading a value from an array
    ArrayLoad {
        /// Reference to the array
        reference: Box<StackItem<'a>>,
        /// Index within the array
        index: Box<StackItem<'a>>,
    },
    /// Represents a negated value
    Negated {
        /// The value being negated
        value: Box<StackItem<'a>>,
    },
    /// Represents the creation of a new object
    New {
        /// The object type
        class: Class<'a>,
    },

    /// Array of primitive type values 1 dimension
    NewArray {
        /// The stack item representing the length of the array
        count: Box<StackItem<'a>>,
        /// The type of the array
        ty: ArrayType,
    },

    /// Array of object references
    ANewArray {
        /// The stack item representing the length of the array
        count: Box<StackItem<'a>>,
        /// The type of the reference
        class: Class<'a>,
    },

    /// Multi-dimensional array of object references
    MultiANewArray {
        /// The length of each array dimension
        counts: Vec<StackItem<'a>>,
        /// The type of the reference
        class: Class<'a>,
    },
    /// Represents an instanceof check
    InstanceOf {
        /// The value to compare
        value: Box<StackItem<'a>>,
        /// The type to compare against
        class: Class<'a>,
    },

    /// Represents a thrown value
    Thrown {
        /// The value that was thrown
        value: Box<StackItem<'a>>,
    },

    /// TODO: Expand JSR and Ret before this step
    Jsr(u16),
    Ret(u16),
}

/// Represents a literal/constant/reference on the stack
#[derive(Debug, Clone)]
pub enum Value<'a> {
    /// Literal null value
    Null,
    /// Literal string value
    String(&'a str),
    /// Literal integer value
    Integer(i32),
    /// Literal float value
    Float(f32),
    /// Literal long value
    Long(i64),
    /// Literal double value
    Double(f64),
}

/// Different operation types
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

/// Represents a method call
#[derive(Debug, Clone)]
pub struct Call<'a> {
    /// Details about the actual method
    pub method: Methodref<'a>,
    /// Stack reference to the method object
    pub reference: Box<StackItem<'a>>,
    /// Stack items for the method arguments
    pub args: Vec<StackItem<'a>>,
}

/// Represents a static method call
#[derive(Debug, Clone)]
pub struct CallStatic<'a> {
    /// Details about the actual method
    pub method: Methodref<'a>,
    /// Stack items for the method arguments
    pub args: Vec<StackItem<'a>>,
}

/// Different value types for a local variabel
#[derive(Debug, Clone)]
pub enum LocalVariableType {
    Int,
    Long,
    Float,
    Double,
    Reference,
}

/// Represents the operand stack
#[derive(Default)]
pub struct Stack<'a> {
    inner: Vec<StackItem<'a>>,
}

/// Represents an error while working on the stack
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

impl<'a> Stack<'a> {
    fn push(&mut self, value: StackItem<'a>) {
        self.inner.push(value);
    }

    fn push_value(&mut self, value: Value<'a>) {
        self.inner.push(StackItem::Value(value));
    }

    fn pop(&mut self) -> StackResult<StackItem<'a>> {
        self.inner.pop().ok_or(StackError::Empty)
    }

    fn pop_discard(&mut self) -> StackResult<()> {
        self.pop()?;
        Ok(())
    }

    fn pop_boxed(&mut self) -> StackResult<Box<StackItem<'a>>> {
        self.pop().map(Box::new)
    }

    fn clone_last(&mut self) -> StackResult<StackItem<'a>> {
        self.inner.last().cloned().ok_or(StackError::Empty)
    }

    fn dup(&mut self) -> StackResult<()> {
        let value = self.clone_last()?;
        self.push(value);
        Ok(())
    }

    fn dup_x1(&mut self) -> StackResult<()> {
        let value = self.clone_last()?;
        self.inner.insert(self.inner.len() - 2, value);
        Ok(())
    }

    fn dup_x2(&mut self) -> StackResult<()> {
        let value = self.clone_last()?;
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

    fn pop2(&mut self) -> StackResult<()> {
        let top = self.pop()?;
        if !matches!(top, StackItem::Value(Value::Double(_) | Value::Long(_))) {
            self.pop()?;
        }

        Ok(())
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

#[derive(Debug)]
pub enum AST<'a> {
    Call(Call<'a>),
    CallStatic(CallStatic<'a>),
    Return {
        /// Value returned (None if void)
        value: Option<StackItem<'a>>,
    },
    /// Conditional statement with a jump to another block
    Condition {
        /// Left hand side of the condition
        left: StackItem<'a>,
        /// Right hand side of the condition
        right: StackItem<'a>,
        /// The actual condition
        ty: ConditionType,
        /// The position to jump to
        jump_index: u16,
    },
    /// Represents storing something in an array
    ArrayStore {
        /// The reference for the array
        reference: StackItem<'a>,
        /// The stack item representing the index within the array
        index: StackItem<'a>,
        /// The value to store at the index
        value: StackItem<'a>,
    },
    /// Represents storing something into a field
    PutField {
        /// The field details
        field: Fieldref<'a>,
        /// The reference to the object the field is on
        reference: StackItem<'a>,
        /// The value to assign the field
        value: StackItem<'a>,
    },
    /// Represents storing something into a static field
    PutFieldStatic {
        /// The field details
        field: Fieldref<'a>,
        /// The value to assign the field
        value: StackItem<'a>,
    },
    /// Represents setting a local variable
    SetLocal {
        /// The index of the local variable
        index: u16,
        /// The value to store in the local variable
        value: StackItem<'a>,
    },
    /// Represents an int increment on a local variable
    LocalIncrement {
        /// The index of the local variable
        index: u16,
        /// The amount to increment by
        value: i16,
    },
    InvokeDynamic(InvokeDynamic),
    /// Represents a thrown value
    Thrown {
        /// The value that was thrown
        value: StackItem<'a>,
    },
    /// TODO: Should this be handled elsewhere?
    LookupSwitch {
        key: StackItem<'a>,
        data: LookupSwitchData,
    },
    /// TODO: Should this be handled elsewhere?
    TableSwitch {
        index: StackItem<'a>,
        data: TableSwitchData,
    },
    /// Plain instruction fallback for errors
    Instruction(Instruction),
}

/// The type of condition
#[derive(Debug)]
pub enum ConditionType {
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
}

/// Errors that could occur while processing instructions
#[derive(Debug, Error)]
pub enum ProcessError {
    #[error("Missing constant at index {0}")]
    MissingConstant(PoolIndex),
    #[error("Unexpected constant pool type at index {0}")]
    UnexpectedConstantType(PoolIndex),
    #[error(transparent)]
    Stack(#[from] StackError),

    #[error("Invalid field reference index {0}")]
    InvalidFieldref(u16),
    #[error("Invalid field reference index {0}")]
    InvalidMethodref(u16),

    #[error("Invalid class reference index {0}")]
    InvalidClassIndex(u16),
    #[error("Class name was not valid")]
    InvalidClassName,

    #[error("Invalid constant index for loading {0}")]
    InvalidConstantIndex(u16),
}

fn process<'a>(
    instruction: &Instruction,
    pool: &ConstantPool<'a>,
    stack: &mut Stack<'a>,
    stms: &mut Vec<AST<'a>>,
) -> Result<(), ProcessError> {
    use classfile::inst::Instruction::*;

    match instruction {
        // Swapping
        Swap => stack.swap()?,

        // Popping
        Pop => stack.pop_discard()?,
        Pop2 => stack.pop2()?,

        // Cast checking
        CheckCast(index) => {
            let class_name: &str = pool
                .get_class_name(*index)
                .ok_or(ProcessError::InvalidClassIndex(*index))?;
            let class: Class =
                Class::try_parse(class_name).ok_or(ProcessError::InvalidClassName)?;
            let value = stack.pop_boxed()?;
            stack.push(StackItem::CheckedCast {
                value,
                cast_to: class,
            })
        }

        // Exception throwing
        AThrow => {
            let value = stack.pop()?;
            stack.push(StackItem::Thrown {
                value: Box::new(value.clone()),
            });
            stms.push(AST::Thrown { value })
        }

        // Constant value pushes
        BIPush(value) => stack.push_value(Value::Integer(*value as i32)),
        SIPush(value) => stack.push_value(Value::Integer(*value as i32)),

        // Constants
        IConst(value) => stack.push_value(Value::Integer(*value)),
        FConst(value) => stack.push_value(Value::Float(*value)),
        DConst(value) => stack.push_value(Value::Double(*value)),
        LConst(value) => stack.push_value(Value::Long(*value)),
        AConstNull => stack.push_value(Value::Null),
        LoadConst(index) => {
            let value = pool
                .get(*index)
                .ok_or(ProcessError::InvalidConstantIndex(*index))?;

            let value = match value {
                ConstantItem::Integer(value) => Value::Integer(*value),
                ConstantItem::Float(value) => Value::Float(*value),
                ConstantItem::Long(value) => Value::Long(*value),
                ConstantItem::Double(value) => Value::Double(*value),
                ConstantItem::String(index) => {
                    let value: &str = pool
                        .get_utf8(*index)
                        .ok_or(ProcessError::InvalidConstantIndex(*index))?;
                    Value::String(value)
                }

                _ => return Err(ProcessError::InvalidConstantIndex(*index)),
            };

            stack.push_value(value);
        }

        // Local variable storing
        AStore(index) | LStore(index) | IStore(index) | DStore(index) | FStore(index) => {
            let index = *index;
            let value = stack.pop()?;
            stms.push(AST::SetLocal { index, value })
        }

        // Local variable loading
        ILoad(index) => stack.push(StackItem::GetLocal {
            index: *index,
            ty: LocalVariableType::Int,
        }),
        LLoad(index) => stack.push(StackItem::GetLocal {
            index: *index,
            ty: LocalVariableType::Long,
        }),
        FLoad(index) => stack.push(StackItem::GetLocal {
            index: *index,
            ty: LocalVariableType::Float,
        }),
        DLoad(index) => stack.push(StackItem::GetLocal {
            index: *index,
            ty: LocalVariableType::Double,
        }),
        ALoad(index) => stack.push(StackItem::GetLocal {
            index: *index,
            ty: LocalVariableType::Reference,
        }),

        // Function invokes
        InvokeSpecial(index) | InvokeVirtual(index) | InvokeInterface(index) => {
            let method: Methodref = pool
                .get_methodref(*index)
                .ok_or(ProcessError::InvalidMethodref(*index))?;

            let mut args = Vec::with_capacity(method.descriptor.parameters.len());
            for _ in 0..args.len() {
                args.push(stack.pop()?);
            }
            args.reverse();

            let reference = stack.pop_boxed()?;

            let call = Call {
                args,
                method,
                reference,
            };

            // TODO: FIGURE OUT IF THIS CALL SHOULD BE PUSHING STACK or AST

            // Non return types should end up on the stack
            if !matches!(&call.method.descriptor.return_type, FieldDesc::Void) {
                stack.push(StackItem::Call(call))
            } else {
                stms.push(AST::Call(call))
            }
        }
        InvokeStatic(index) => {
            let method: Methodref = pool
                .get_methodref(*index)
                .ok_or(ProcessError::InvalidMethodref(*index))?;

            let mut args = Vec::with_capacity(method.descriptor.parameters.len());
            for _ in 0..args.len() {
                args.push(stack.pop()?);
            }
            args.reverse();
            let call = CallStatic { args, method };

            // TODO: FIGURE OUT IF THIS CALL SHOULD BE PUSHING STACK or AST

            // Non return types should end up on the stack
            if !matches!(&call.method.descriptor.return_type, FieldDesc::Void) {
                stack.push(StackItem::CallStatic(call))
            } else {
                stms.push(AST::CallStatic(call))
            }
        }

        // Storing in fields
        PutField(index) => {
            let field: Fieldref<'a> = pool
                .get_fieldref(*index)
                .ok_or(ProcessError::InvalidFieldref(*index))?;

            let value: StackItem = stack.pop()?;
            let reference: StackItem = stack.pop()?;

            stms.push(AST::PutField {
                field,
                reference,
                value,
            })
        }
        PutStatic(index) => {
            let field: Fieldref = pool
                .get_fieldref(*index)
                .ok_or(ProcessError::InvalidFieldref(*index))?;

            let value: StackItem = stack.pop()?;

            stms.push(AST::PutFieldStatic { field, value })
        }

        // Retreiving from fields
        GetField(index) => {
            let field: Fieldref = pool
                .get_fieldref(*index)
                .ok_or(ProcessError::InvalidFieldref(*index))?;
            let reference: Box<StackItem> = stack.pop_boxed()?;
            stack.push(StackItem::GetField { field, reference })
        }
        GetStatic(index) => {
            let field: Fieldref = pool
                .get_fieldref(*index)
                .ok_or(ProcessError::InvalidFieldref(*index))?;
            stack.push(StackItem::GetFieldStatic { field })
        }

        // Array length access
        ArrayLength => {
            let reference: Box<StackItem> = stack.pop_boxed()?;
            stack.push(StackItem::ArrayLength { reference })
        }

        // Returning
        Return => stms.push(AST::Return { value: None }),
        AReturn | DReturn | FReturn | IReturn | LReturn => {
            let value: StackItem = stack.pop()?;
            stms.push(AST::Return { value: Some(value) })
        }

        // Stack duplicating
        Dup => stack.dup()?,
        DupX1 => stack.dup_x1()?,
        DupX2 => stack.dup_x2()?,
        Dup2 => stack.dup_2()?,
        Dup2X1 => stack.dup_2x1()?,
        Dup2X2 => stack.dup_2x2()?,

        // Array loading
        IALoad | LALoad | DALoad | FALoad | SALoad | CALoad | BALoad | AALoad => {
            let index: Box<StackItem> = stack.pop_boxed()?;
            let reference: Box<StackItem> = stack.pop_boxed()?;

            stack.push(StackItem::ArrayLoad { reference, index })
        }

        // Array storing
        IAStore | LAStore | DAStore | FAStore | SAStore | CAStore | BAStore | AAStore => {
            let value: StackItem = stack.pop()?;
            let index: StackItem = stack.pop()?;
            let reference: StackItem = stack.pop()?;
            stms.push(AST::ArrayStore {
                reference,
                index,
                value,
            })
        }

        // Adding
        IAdd | LAdd | FAdd | DAdd => {
            let right: Box<StackItem> = stack.pop_boxed()?;
            let left: Box<StackItem> = stack.pop_boxed()?;
            stack.push(StackItem::Operation {
                right,
                ty: OperationType::Add,
                left,
            })
        }

        // Subtracting
        ISub | LSub | FSub | DSub => {
            let right: Box<StackItem> = stack.pop_boxed()?;
            let left: Box<StackItem> = stack.pop_boxed()?;
            stack.push(StackItem::Operation {
                right,
                ty: OperationType::Subtract,
                left,
            })
        }

        // Dividing
        IDiv | LDiv | FDiv | DDiv => {
            let right: Box<StackItem> = stack.pop_boxed()?;
            let left: Box<StackItem> = stack.pop_boxed()?;
            stack.push(StackItem::Operation {
                right,
                ty: OperationType::Divide,
                left,
            })
        }

        // Multiplying
        IMul | LMul | FMul | DMul => {
            let right: Box<StackItem> = stack.pop_boxed()?;
            let left: Box<StackItem> = stack.pop_boxed()?;
            stack.push(StackItem::Operation {
                right,
                ty: OperationType::Muliply,
                left,
            })
        }

        // Remainder
        IRem | LRem | FRem | DRem => {
            let right: Box<StackItem> = stack.pop_boxed()?;
            let left: Box<StackItem> = stack.pop_boxed()?;
            stack.push(StackItem::Operation {
                right,
                ty: OperationType::Remainder,
                left,
            })
        }

        // Bitwise And
        IAnd | LAnd => {
            let right: Box<StackItem> = stack.pop_boxed()?;
            let left: Box<StackItem> = stack.pop_boxed()?;
            stack.push(StackItem::Operation {
                right,
                ty: OperationType::BitwiseAnd,
                left,
            })
        }

        // | Bitwise OR operation
        LOr | IOr => {
            let right: Box<StackItem> = stack.pop_boxed()?;
            let left: Box<StackItem> = stack.pop_boxed()?;
            stack.push(StackItem::Operation {
                right,
                ty: OperationType::BitwiseOr,
                left,
            })
        }

        // ^ XOR operation
        LXOr | IXOr => {
            let right: Box<StackItem> = stack.pop_boxed()?;
            let left: Box<StackItem> = stack.pop_boxed()?;
            stack.push(StackItem::Operation {
                right,
                ty: OperationType::Xor,
                left,
            })
        }

        // << Bitwise shift left operation
        IShL | LShL => {
            let right: Box<StackItem> = stack.pop_boxed()?;
            let left: Box<StackItem> = stack.pop_boxed()?;
            stack.push(StackItem::Operation {
                right,
                ty: OperationType::BitwiseShl,
                left,
            })
        }

        // >> Bitwise shift right operation
        IShR | LShR => {
            let right: Box<StackItem> = stack.pop_boxed()?;
            let left: Box<StackItem> = stack.pop_boxed()?;
            stack.push(StackItem::Operation {
                right,
                ty: OperationType::BitwiseShr,
                left,
            })
        }

        // >>> Logical shift right operation
        IUShR | LUShR => {
            let right: Box<StackItem> = stack.pop_boxed()?;
            let left: Box<StackItem> = stack.pop_boxed()?;
            stack.push(StackItem::Operation {
                right,
                ty: OperationType::LogicalShr,
                left,
            })
        }

        // < Less than comparison (float, double)
        FCmpL | DCmpL => {
            let right: Box<StackItem> = stack.pop_boxed()?;
            let left: Box<StackItem> = stack.pop_boxed()?;
            stack.push(StackItem::Operation {
                right,
                ty: OperationType::CompareLess,
                left,
            })
        }

        // > Greater than comparison (float, double)
        FCmpG | DCmpG => {
            let right: Box<StackItem> = stack.pop_boxed()?;
            let left: Box<StackItem> = stack.pop_boxed()?;
            stack.push(StackItem::Operation {
                right,
                ty: OperationType::CompareGreater,
                left,
            })
        }

        // Negating
        INeg | LNeg | FNeg | DNeg => {
            let value = stack.pop_boxed()?;
            stack.push(StackItem::Negated { value })
        }

        // Int casting
        L2i | D2i | F2i => {
            let value = stack.pop_boxed()?;
            stack.push(StackItem::LiteralCast {
                value,
                cast_to: FieldDesc::Int,
            })
        }

        // Long casting
        I2l | F2l | D2l => {
            let value = stack.pop_boxed()?;
            stack.push(StackItem::LiteralCast {
                value,
                cast_to: FieldDesc::Long,
            })
        }

        // Float casting
        D2f | L2f | I2f => {
            let value = stack.pop_boxed()?;
            stack.push(StackItem::LiteralCast {
                value,
                cast_to: FieldDesc::Float,
            })
        }

        // Double casting
        I2d | L2d | F2d => {
            let value = stack.pop_boxed()?;
            stack.push(StackItem::LiteralCast {
                value,
                cast_to: FieldDesc::Double,
            })
        }

        I2s => {
            let value = stack.pop_boxed()?;
            stack.push(StackItem::LiteralCast {
                value,
                cast_to: FieldDesc::Short,
            })
        }
        I2c => {
            let value = stack.pop_boxed()?;
            stack.push(StackItem::LiteralCast {
                value,
                cast_to: FieldDesc::Char,
            })
        }
        I2b => {
            let value = stack.pop_boxed()?;
            stack.push(StackItem::LiteralCast {
                value,
                cast_to: FieldDesc::Byte,
            })
        }

        // Switches
        LookupSwitch(data) => {
            let key = stack.pop()?;
            stms.push(AST::LookupSwitch {
                key,
                data: data.clone(),
            })
        }
        TableSwitch(data) => {
            let index = stack.pop()?;
            stms.push(AST::TableSwitch {
                index,
                data: data.clone(),
            })
        }

        // Monitoring (TODO: Find actual right way)
        MonitorEnter | MonitorExit => stack.pop_discard()?,

        // New instance creation
        New(index) => {
            let class_name: &str = pool
                .get_class_name(*index)
                .ok_or(ProcessError::InvalidClassIndex(*index))?;
            let class: Class =
                Class::try_parse(class_name).ok_or(ProcessError::InvalidClassName)?;
            stack.push(StackItem::New { class });
        }

        // Array creation
        NewArray(ty) => {
            let count: Box<StackItem> = stack.pop_boxed()?;

            stack.push(StackItem::NewArray { count, ty: *ty });
        }
        ANewArray(index) => {
            let class_name: &str = pool
                .get_class_name(*index)
                .ok_or(ProcessError::InvalidClassIndex(*index))?;
            let class: Class =
                Class::try_parse(class_name).ok_or(ProcessError::InvalidClassName)?;
            let count: Box<StackItem> = stack.pop_boxed()?;
            stack.push(StackItem::ANewArray { count, class });
        }
        MultiANewArray(data) => {
            let class_name: &str = pool
                .get_class_name(data.index)
                .ok_or(ProcessError::InvalidClassIndex(data.index))?;
            let class: Class =
                Class::try_parse(class_name).ok_or(ProcessError::InvalidClassName)?;
            let mut counts: Vec<StackItem> = Vec::with_capacity(data.dimensions as usize);
            for _ in 0..data.dimensions {
                counts.push(stack.pop()?);
            }
            counts.reverse();

            stack.push(StackItem::MultiANewArray { counts, class });
        }

        // Instance checking
        InstanceOf(index) => {
            let class_name: &str = pool
                .get_class_name(*index)
                .ok_or(ProcessError::InvalidClassIndex(*index))?;
            let class: Class =
                Class::try_parse(class_name).ok_or(ProcessError::InvalidClassName)?;
            let value: Box<StackItem> = stack.pop_boxed()?;

            stack.push(StackItem::InstanceOf { value, class })
        }

        // Incrementing
        IInc(data) => stms.push(AST::LocalIncrement {
            index: data.index,
            value: data.value,
        }),

        // invoke dynamic
        InvokeDynamic(index) => {
            let dynamic = pool.get_invokedynamic(*index).unwrap();
            stms.push(AST::InvokeDynamic(dynamic))
        }

        // Reference compare
        IfACmpEq(index) => {
            let right: StackItem = stack.pop()?;
            let left: StackItem = stack.pop()?;
            stms.push(AST::Condition {
                right,
                left,
                ty: ConditionType::Equal,
                jump_index: *index,
            })
        }
        IfACmpNe(index) => {
            let right: StackItem = stack.pop()?;
            let left: StackItem = stack.pop()?;
            stms.push(AST::Condition {
                right,
                left,
                ty: ConditionType::NotEqual,
                jump_index: *index,
            })
        }

        // Int compare
        IfICmpEq(index) => {
            let right: StackItem = stack.pop()?;
            let left: StackItem = stack.pop()?;
            stms.push(AST::Condition {
                right,
                left,
                ty: ConditionType::Equal,
                jump_index: *index,
            })
        }
        IfICmpNe(index) => {
            let right: StackItem = stack.pop()?;
            let left: StackItem = stack.pop()?;
            stms.push(AST::Condition {
                right,
                left,
                ty: ConditionType::NotEqual,
                jump_index: *index,
            })
        }
        IfICmpLt(index) => {
            let right: StackItem = stack.pop()?;
            let left: StackItem = stack.pop()?;
            stms.push(AST::Condition {
                right,
                left,
                ty: ConditionType::LessThan,
                jump_index: *index,
            })
        }
        IfICmpLe(index) => {
            let right: StackItem = stack.pop()?;
            let left: StackItem = stack.pop()?;
            stms.push(AST::Condition {
                right,
                left,
                ty: ConditionType::LessThanOrEqual,
                jump_index: *index,
            })
        }
        IfICmpGt(index) => {
            let right: StackItem = stack.pop()?;
            let left: StackItem = stack.pop()?;
            stms.push(AST::Condition {
                right,
                left,
                ty: ConditionType::GreaterThan,
                jump_index: *index,
            })
        }

        IfICmpGe(index) => {
            let right: StackItem = stack.pop()?;
            let left: StackItem = stack.pop()?;
            stms.push(AST::Condition {
                right,
                left,
                ty: ConditionType::GreaterThanOrEqual,
                jump_index: *index,
            })
        }

        // Long compare
        LCmp => {
            let right = stack.pop_boxed()?;
            let left = stack.pop_boxed()?;
            stack.push(StackItem::Operation {
                right,
                ty: OperationType::SignedCompare,
                left,
            })
        }

        // Null compare
        IfNull(index) => {
            let right: StackItem = stack.pop()?;
            let left: StackItem = StackItem::Value(Value::Null);
            stms.push(AST::Condition {
                right,
                left,
                ty: ConditionType::Equal,
                jump_index: *index,
            })
        }
        IfNonNull(index) => {
            let right: StackItem = stack.pop()?;
            let left: StackItem = StackItem::Value(Value::Null);
            stms.push(AST::Condition {
                right,
                left,
                ty: ConditionType::NotEqual,
                jump_index: *index,
            })
        }

        // Int zero compare
        IfEq(index) => {
            let right: StackItem = stack.pop()?;
            let left: StackItem = StackItem::Value(Value::Integer(0));
            stms.push(AST::Condition {
                right,
                left,
                ty: ConditionType::Equal,
                jump_index: *index,
            })
        }
        IfNe(index) => {
            let right: StackItem = stack.pop()?;
            let left: StackItem = StackItem::Value(Value::Integer(0));
            stms.push(AST::Condition {
                right,
                left,
                ty: ConditionType::NotEqual,
                jump_index: *index,
            })
        }
        IfLt(index) => {
            let right: StackItem = stack.pop()?;
            let left: StackItem = StackItem::Value(Value::Integer(0));
            stms.push(AST::Condition {
                right,
                left,
                ty: ConditionType::LessThan,
                jump_index: *index,
            })
        }
        IfLe(index) => {
            let right: StackItem = stack.pop()?;
            let left: StackItem = StackItem::Value(Value::Integer(0));
            stms.push(AST::Condition {
                right,
                left,
                ty: ConditionType::LessThanOrEqual,
                jump_index: *index,
            })
        }
        IfGe(index) => {
            let right: StackItem = stack.pop()?;
            let left: StackItem = StackItem::Value(Value::Integer(0));
            stms.push(AST::Condition {
                right,
                left,
                ty: ConditionType::GreaterThanOrEqual,
                jump_index: *index,
            })
        }
        IfGt(index) => {
            let right: StackItem = stack.pop()?;
            let left: StackItem = StackItem::Value(Value::Integer(0));
            stms.push(AST::Condition {
                right,
                left,
                ty: ConditionType::GreaterThan,
                jump_index: *index,
            })
        }

        JSr(index) => stack.push(StackItem::Jsr(*index)),
        Ret(index) => stack.push(StackItem::Ret(*index)),

        Goto(_) | Nop => {}
    }

    Ok(())
}
