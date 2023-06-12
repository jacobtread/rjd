use std::collections::HashMap;

use classfile::{
    attributes::{BorrowedInstrSet, InstructionSet},
    constant_pool::{ConstantPool, Fieldref, InvokeDynamic, Methodref},
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
    Value(Value<'a>),
    Casted(Casted<'a>),
    Operation(Operation<'a>),
    Call(Call<'a>),
    CallStatic(CallStatic<'a>),
    LocalVariable(LocalVariable),
    GetField(GetField<'a>),
    GetFieldStatic(GetFieldStatic<'a>),
    ArrayLength(ArrayLength<'a>),
    ArrayLoad(ArrayLoad<'a>),
    Negated(Box<StackItem<'a>>),

    /// TODO: Expand JSR and Ret before this step
    Jsr(u16),
    Ret(u16),
}

/// Represents one type being cast to another
#[derive(Debug, Clone)]
pub struct Casted<'a> {
    /// The value being cast
    pub value: Box<StackItem<'a>>,
    /// The type that it was cast to
    pub cast_to: FieldDesc<'a>,
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

/// Represents an operation between two stack items
#[derive(Debug, Clone)]
pub struct Operation<'a> {
    /// The left hand side of the operation
    pub left: Box<StackItem<'a>>,
    /// The type of operation
    pub ty: OperationType,
    /// The right hand side of the operation
    pub right: Box<StackItem<'a>>,
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

/// Represents a local variable on the stack
#[derive(Debug, Clone)]
pub struct LocalVariable {
    /// The index of the local variable
    pub index: u16,
    /// The type of the local variable
    pub ty: LocalVariableType,
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

/// Represents getting a field value from an object
#[derive(Debug, Clone)]
pub struct GetField<'a> {
    /// The reference to the field
    pub field: Fieldref<'a>,
    /// Reference to the object the field is on
    pub reference: Box<StackItem<'a>>,
}

/// Represents getting a static field value
#[derive(Debug, Clone)]
pub struct GetFieldStatic<'a> {
    /// The reference to the field
    pub field: Fieldref<'a>,
}

/// Represents access of the .length field on an array
#[derive(Debug, Clone)]
pub struct ArrayLength<'a> {
    /// Reference to the array
    pub reference: Box<StackItem<'a>>,
}

/// Represents loading a value from an array
#[derive(Debug, Clone)]
pub struct ArrayLoad<'a> {
    /// Reference to the array
    pub reference: Box<StackItem<'a>>,
    /// Index within the array
    pub index: Box<StackItem<'a>>,
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

    fn pop(&mut self) -> StackResult<StackItem<'a>> {
        self.inner.pop().ok_or(StackError::Empty)
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
    Return(Option<StackItem<'a>>),
    Condition(Condition<'a>),
    ArrayStore(ArrayStore<'a>),
    PutField(PutField<'a>),
    PutFieldStatic(PutFieldStatic<'a>),
    SetLocal(SetLocal<'a>),
    LocalIncrement(LocalIncrement),
    InvokeDynamic(InvokeDynamic),
    Throw(StackItem<'a>),
    LookupSwitch(LookupSwitchImpl<'a>),
    TableSwitch(TableSwitchImpl<'a>),
}

/// Conditional statement with a jump to another block
#[derive(Debug)]
pub struct Condition<'a> {
    /// Left hand side of the condition
    pub left: StackItem<'a>,
    /// Right hand side of the condition
    pub right: StackItem<'a>,
    /// The actual condition
    pub ty: ConditionType,
    /// The position to jump to
    pub jump_index: u16,
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

/// Represents storing something in an array
#[derive(Debug)]
pub struct ArrayStore<'a> {
    /// The reference for the array
    pub reference: StackItem<'a>,
    /// The stack item representing the index within the array
    pub index: StackItem<'a>,
    /// The value to store at the index
    pub value: StackItem<'a>,
}

/// Represents storing something into a field
#[derive(Debug)]
pub struct PutField<'a> {
    /// The field details
    pub field: Fieldref<'a>,
    /// The reference to the object the field is on
    pub reference: StackItem<'a>,
    /// The value to assign the field
    pub value: StackItem<'a>,
}

/// Represents storing something into a static field
#[derive(Debug)]
pub struct PutFieldStatic<'a> {
    /// The field details
    pub field: Fieldref<'a>,
    /// The value to assign the field
    pub value: StackItem<'a>,
}

/// Represents setting a local variable
#[derive(Debug)]
pub struct SetLocal<'a> {
    /// The index of the local variable
    pub index: u16,
    /// The value to store in the local variable
    pub value: StackItem<'a>,
}

/// Represents an int increment on a local variable
#[derive(Debug)]
pub struct LocalIncrement {
    /// The index of the local variable
    pub index: u16,
    /// The amount to increment by
    pub value: u16,
}

/// TODO: Should this be handled elsewhere
#[derive(Debug)]
pub struct LookupSwitchImpl<'a> {
    pub key: StackItem<'a>,
    pub data: LookupSwitchData,
}

/// TODO: Should this be handled elsewhere
#[derive(Debug)]
pub struct TableSwitchImpl<'a> {
    pub key: StackItem<'a>,
    pub data: TableSwitchData,
}

fn process<'a>(
    instruction: &Instruction,
    pool: &ConstantPool<'a>,
    stack: &mut Stack<'a>,
) -> Result<(), ()> {
    use classfile::inst::Instruction::*;

    Ok(())
}
