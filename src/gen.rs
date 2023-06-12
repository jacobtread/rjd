use std::collections::HashMap;

use classfile::{
    attributes::{BorrowedInstrSet, InstructionSet},
    constant_pool::{ConstantPool, Fieldref, InvokeDynamic, Methodref, PoolIndex},
    inst::{ArrayType, Instruction, LookupSwitchData, TableSwitchData},
    types::{Class, FieldDesc},
};
use thiserror::Error;

use crate::ast::{pinstr, Arrayref, InstrError};

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
    ) -> Result<Vec<AST<'a>>, InstrError> {
        let mut stack: Stack<'a> = Stack::default();
        let mut ast: Vec<AST> = Vec::new();

        // let mut iter = self.instructions.inner.iter();

        // for (_pos, instr) in iter.by_ref() {
        //     if let Err(err) = pinstr(instr, pool, &mut stack, &mut ast) {
        //         eprintln!("ERR: {:?}", err);
        //         ast.push(AST::Other(instr.clone()));
        //         break;
        //     }
        // }

        // for (_pos, instr) in iter {
        //     ast.push(AST::Other(instr.clone()))
        // }

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
    /// Represents a cast of a value to a type
    Casted {
        /// The value being cast
        value: Box<StackItem<'a>>,
        /// The type that it was cast to
        cast_to: FieldDesc<'a>,
    },
    /// Represents an operation between two stack items
    Operation {
        /// The left hand side of the operation
        left: Box<StackItem<'a>>,
        /// The type of operation
        ty: OperationType,
        /// The right hand side of the operation
        right: Box<StackItem<'a>>,
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
    /// Reference to an object type
    Objectref(Class<'a>),
    /// Reference to an array type
    Arrayref(Arrayref<'a>),
}

/// Types of array references
#[derive(Debug, Clone)]
pub enum ArrayRef<'a> {
    /// Array of primitive type values 1 dimension
    Primitive {
        /// The stack item representing the length of the array
        count: Box<StackItem<'a>>,
        ty: ArrayType,
    },
    /// Array of object references
    Reference {
        /// The stack item representing the length of the array
        count: Box<StackItem<'a>>,
        /// The type of the reference
        ty: Class<'a>,
    },
    /// Multi-dimensional array of object references
    MultiReferenceArray {
        /// The length of each array dimension
        counts: Vec<StackItem<'a>>,
        /// The type of the reference
        ty: Class<'a>,
    },
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
        value: u16,
    },
    InvokeDynamic(InvokeDynamic),
    Throw {
        value: StackItem<'a>,
    },
    /// TODO: Should this be handled elsewhere?
    LookupSwitch {
        key: StackItem<'a>,
        data: LookupSwitchData,
    },
    /// TODO: Should this be handled elsewhere?
    TableSwitch {
        key: StackItem<'a>,
        data: TableSwitchData,
    },
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
        CheckCast(_) => todo!(),

        // Exception throwing
        AThrow => todo!(),

        // Constant value pushes
        BIPush(value) => stack.push_value(Value::Integer(*value as i32)),
        SIPush(value) => stack.push_value(Value::Integer(*value as i32)),

        // Constants
        IConst(value) => stack.push_value(Value::Integer(*value)),
        FConst(value) => stack.push_value(Value::Float(*value)),
        DConst(value) => stack.push_value(Value::Double(*value)),
        LConst(value) => stack.push_value(Value::Long(*value)),
        AConstNull => stack.push_value(Value::Null),
        LoadConst(_) => todo!(),

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
        InvokeSpecial(_) => todo!(),
        InvokeStatic(_) => todo!(),
        InvokeVirtual(_) => todo!(),
        InvokeInterface(_) => todo!(),

        // Storing in fields
        PutField(index) => {
            let field: Fieldref<'a> = pool
                .get_fieldref(*index)
                .ok_or(ProcessError::InvalidFieldref(*index))?;

            let reference: StackItem = stack.pop()?;
            let value: StackItem = stack.pop()?;

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
            let value = stack.pop()?;
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
        IALoad => todo!(),
        LALoad => todo!(),
        DALoad => todo!(),
        FALoad => todo!(),
        SALoad => todo!(),
        CALoad => todo!(),
        BALoad => todo!(),
        AALoad => todo!(),

        // Array storing
        IAStore => todo!(),
        LAStore => todo!(),
        DAStore => todo!(),
        FAStore => todo!(),
        SAStore => todo!(),
        CAStore => todo!(),
        BAStore => todo!(),
        AAStore => todo!(),

        // Adding
        IAdd => todo!(),
        LAdd => todo!(),
        FAdd => todo!(),
        DAdd => todo!(),

        // Subtracting
        ISub => todo!(),
        LSub => todo!(),
        FSub => todo!(),
        DSub => todo!(),

        // Dividing
        IDiv => todo!(),
        LDiv => todo!(),
        FDiv => todo!(),
        DDiv => todo!(),

        // Multiplying
        IMul => todo!(),
        LMul => todo!(),
        FMul => todo!(),
        DMul => todo!(),

        // Negating
        INeg => todo!(),
        LNeg => todo!(),
        FNeg => todo!(),
        DNeg => todo!(),

        // Remainder
        IRem => todo!(),
        LRem => todo!(),
        FRem => todo!(),
        DRem => todo!(),

        // Bitwise And
        IAnd => todo!(),
        LAnd => todo!(),

        // | Bitwise OR operation
        LOr => todo!(),
        IOr => todo!(),

        // ^ XOR operation
        LXOr => todo!(),
        IXOr => todo!(),

        FCmpL => todo!(),
        FCmpG => todo!(),

        // Int casting
        I2l => todo!(),
        I2d => todo!(),
        I2s => todo!(),
        I2c => todo!(),
        I2b => todo!(),
        I2f => todo!(),
        // Long Casting
        L2i => todo!(),
        L2d => todo!(),
        L2f => todo!(),
        // Float Casting
        F2d => todo!(),
        F2i => todo!(),
        F2l => todo!(),
        // Double casting
        D2i => todo!(),
        D2f => todo!(),
        D2l => todo!(),

        // << Bitwise shift left operation
        IShL => todo!(),
        LShL => todo!(),

        // >> Bitwise shift right operation
        IShR => todo!(),
        LShR => todo!(),

        // >>> Logical shift right operation
        IUShR => todo!(),
        LUShR => todo!(),

        // Switches
        TableSwitch(_) => todo!(),
        LookupSwitch(_) => todo!(),

        // Monitoring
        MonitorEnter => todo!(),
        MonitorExit => todo!(),

        // New instance creation
        New(_) => todo!(),

        // Array creation
        NewArray(_) => todo!(),
        ANewArray(_) => todo!(),
        MultiANewArray(data) => todo!(),

        // Instance checking
        InstanceOf(_) => todo!(),

        // Incrementing
        IInc(data) => todo!(),

        // invoke dynamic
        InvokeDynamic(_) => todo!(),

        // Reference compare
        IfACmpEq(_) => todo!(),
        IfACmpNe(_) => todo!(),

        // Int compare
        IfICmpEq(_) => todo!(),
        IfICmpNe(_) => todo!(),
        IfICmpLt(_) => todo!(),
        IfICmpGe(_) => todo!(),
        IfICmpGt(_) => todo!(),
        IfICmpLe(_) => todo!(),

        // Double compare
        DCmpL => todo!(),
        DCmpG => todo!(),

        // Long compare
        LCmp => todo!(),

        // Null compare
        IfNull(_) => todo!(),
        IfNonNull(_) => todo!(),

        // Int zero compare
        IfEq(_) => todo!(),
        IfNe(_) => todo!(),
        IfLt(_) => todo!(),
        IfGe(_) => todo!(),
        IfGt(_) => todo!(),
        IfLe(_) => todo!(),

        Goto(_) => todo!(),
        JSr(_) => todo!(),
        Ret(_) => todo!(),
        Nop => todo!(),
    }

    Ok(())
}
