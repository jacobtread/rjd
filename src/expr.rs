use std::{
    collections::HashMap,
    fmt::{Display, Write},
};

use classfile::{
    attributes::{BorrowedInstrSet, InstructionSeq},
    constant_pool::{
        ConstantItem, ConstantPool, Fieldref, MethodNameAndType, Methodref, PoolIndex,
    },
    inst::{ArrayType, Instruction},
    types::{Class, FieldDesc},
};
use thiserror::Error;

/// Block of instructions
#[derive(Debug)]
pub struct Block<'set> {
    pub start: usize,
    pub instructions: BorrowedInstrSet<'set>,
    pub branches: Vec<usize>,
}

impl<'set> Block<'set> {
    pub fn decompile<'a, 'b: 'a>(
        &'b self,
        pool: &'b ConstantPool<'a>,
    ) -> Result<Vec<Exprent<'a>>, ProcessError> {
        let mut stack: Stack<'a> = Stack::default();
        let mut ast: Vec<Exprent<'a>> = Vec::new();

        let mut iter = self.instructions.inner.iter();

        for (_pos, instr) in iter.by_ref() {
            if let Err(err) = process(instr, pool, &mut stack, &mut ast) {
                eprintln!("ERR: {:?}", err);
                break;
            }
        }

        Ok(ast)
    }
}

pub fn create_blocks(input: &InstructionSeq) -> HashMap<usize, Block<'_>> {
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
                TableSwitch { offsets, .. } => {
                    for offset in offsets {
                        let jump = *offset as usize;
                        branches.push(jump)
                    }
                }
                LookupSwitch { pairs, .. } => {
                    for (_, offset) in pairs {
                        let jump = *offset as usize;
                        branches.push(jump)
                    }
                }
                _ => {
                    let next_pos = next.unwrap();
                    branches.push(next_pos);
                }
            }

            (
                first_pos,
                Block {
                    start: first_pos,
                    instructions: set,
                    branches,
                },
            )
        })
        .collect()
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
    /// Object class type
    Object(Class<'a>),
}

impl Value<'_> {
    /// Obtains category of type as per:
    /// https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-2.html#jvms-2.11.1-320
    fn category(&self) -> u8 {
        match self {
            Self::Long(_) | Self::Double(_) => 2,
            _ => 1,
        }
    }
}

impl Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null => f.write_str("null"),
            Self::String(value) => {
                f.write_char('"')?;
                f.write_str(value)?;
                f.write_char('"')
            }
            Self::Integer(value) => value.fmt(f),
            Self::Float(value) => {
                value.fmt(f)?;
                f.write_char('F')
            }
            Self::Long(value) => {
                value.fmt(f)?;
                f.write_char('L')
            }
            Self::Double(value) => value.fmt(f),
            Self::Object(value) => value.class.fmt(f),
        }
    }
}

/// Different operation types
#[derive(Debug, Clone)]
pub enum OperationType<'a> {
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
    Increment,
    InstanceOf,
    Negate,
    Cast(FieldDesc<'a>),
    ArrayLength,
}

enum Position {
    Before,
    After,
}

impl<'a> OperationType<'a> {
    fn position(&self) -> Position {
        match self {
            Self::ArrayLength => Position::After,
            _ => Position::Before,
        }
    }
}

impl Display for OperationType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Muliply => f.write_char('*'),
            Self::Divide => f.write_char('/'),
            Self::Subtract | Self::Negate => f.write_char('-'),
            Self::Add => f.write_char('+'),
            Self::CompareGreater => f.write_char('>'),
            Self::CompareLess => f.write_char('<'),
            Self::SignedCompare => f.write_str("=="),
            Self::Xor => f.write_char('^'),
            Self::BitwiseAnd => f.write_char('&'),
            Self::BitwiseOr => f.write_char('|'),
            Self::BitwiseShl => f.write_str("<<"),
            Self::BitwiseShr => f.write_str(">>"),
            Self::LogicalShr => f.write_str(">>>"),
            Self::Remainder => f.write_char('%'),
            Self::Increment => f.write_str("+="),
            Self::InstanceOf => f.write_str("instanceof"),
            Self::Cast(cast_to) => write!(f, "({})", cast_to),
            Self::ArrayLength => f.write_str(".length"),
        }
    }
}

/// Different value types for a local variabel
#[derive(Debug, Clone)]
pub enum LocalVariableType {
    Int,
    Long,
    Float,
    Double,
    Reference,
    Unknown,
}

impl LocalVariableType {
    fn category(&self) -> u8 {
        match self {
            Self::Double | Self::Long => 2,
            _ => 1,
        }
    }
}

/// Represents the operand stack
#[derive(Default)]
pub struct Stack<'a> {
    inner: Vec<Exprent<'a>>,
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
    fn push(&mut self, value: Exprent<'a>) {
        self.inner.push(value);
    }

    pub fn pop_use(&mut self, stms: &mut Vec<Exprent<'a>>) -> StackResult<()> {
        let value = self.pop()?;

        if let Exprent::Invoke { .. } = &value {
            stms.push(value)
        }
        Ok(())
    }

    pub fn pop2_use(&mut self, stms: &mut Vec<Exprent<'a>>) -> StackResult<()> {
        let cat = self
            .inner
            .last()
            .map(|value| value.category())
            .ok_or(StackError::Empty)?;

        if cat == 1 {
            self.pop_use(stms)?;
        }

        self.pop_use(stms)
    }

    fn push_value(&mut self, value: Value<'a>) {
        self.inner.push(Exprent::Value(value));
    }

    fn pop(&mut self) -> StackResult<Exprent<'a>> {
        self.inner.pop().ok_or(StackError::Empty)
    }

    fn pop_discard(&mut self) -> StackResult<()> {
        self.pop()?;
        Ok(())
    }

    fn pop_boxed(&mut self) -> StackResult<Box<Exprent<'a>>> {
        self.pop().map(Box::new)
    }

    fn clone_last(&mut self) -> StackResult<Exprent<'a>> {
        self.inner.last().cloned().ok_or(StackError::Empty)
    }

    fn dup(&mut self) -> StackResult<()> {
        let value = self.clone_last()?;
        self.push(value);
        Ok(())
    }

    // Duplicate last value inserting it before the last value
    fn dup_x1(&mut self) -> StackResult<()> {
        let length = self.inner.len();
        if length < 2 {
            return Err(StackError::NotEnough {
                required: 2,
                length,
            });
        }

        // Duplicate the last value
        let value = self.inner[length - 1].clone();
        // Insert it before the last value
        self.inner.insert(length - 2, value);
        Ok(())
    }

    fn dup_x2(&mut self) -> StackResult<()> {
        let length = self.inner.len();
        if length < 2 {
            return Err(StackError::NotEnough {
                required: 2,
                length,
            });
        }

        // Duplicate the last value
        let value = self.inner[length - 1].clone();

        let last_x2 = &self.inner[length - 2];
        if last_x2.category() == 2 {
            self.inner.insert(length - 2, value);
        } else {
            self.inner.insert(length - 3, value);
        }

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

        let last = self.inner[length - 1].clone();

        if last.category() == 2 {
            self.push(last);
        } else {
            let last_x2 = self.inner[length - 2].clone();
            self.push(last);
            self.push(last_x2);
        }

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

        let last = self.inner[length - 1].clone();

        if last.category() == 2 {
            self.inner.insert(length - 2, last);
        } else {
            if length < 3 {
                return Err(StackError::NotEnough {
                    required: 3,
                    length,
                });
            }

            let last_x2 = self.inner[length - 2].clone();

            self.inner.insert(length - 3, last);
            self.inner.insert(length - 3, last_x2);
        }

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

        let last = self.inner[length - 1].clone();
        let last_x2 = &self.inner[length - 2];

        if last.category() == 2 {
            if last_x2.category() == 2 {
                self.inner.insert(length - 2, last);
            } else {
                self.inner.insert(length - 3, last);
            }
        } else {
            if length < 3 {
                return Err(StackError::NotEnough {
                    required: 3,
                    length,
                });
            }
            let last_x3 = &self.inner[length - 3];
            if last_x3.category() == 2 {
                self.inner.insert(length - 3, last_x2.clone());
                self.inner.insert(length - 3, last);
            } else {
                self.inner.insert(length - 4, last_x2.clone());
                self.inner.insert(length - 4, last);
            }
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

/// Exit types
#[derive(Debug, Clone)]
pub enum ExitType {
    /// Method returned
    Return,
    /// Exception was thrown
    Throw,
}

/// Different types of invocations
#[derive(Debug, Clone)]
pub enum InvokeType {
    Special,
    Virtual,
    Static,
    Interface,
    Dynamic,
}

#[derive(Debug, Clone)]
pub enum Exprent<'a> {
    /// Represents an actual value
    Value(Value<'a>),

    /// Represents a function invokation
    Invoke {
        /// Method name, class and descriptor
        method: Methodref<'a>,
        /// Reference to the object the function is apart of
        /// (None if the invocation is static)
        reference: Option<Box<Exprent<'a>>>,
        /// Arguments provided to the method call
        args: Vec<Exprent<'a>>,
        /// The type of invocation
        ty: InvokeType,
    },

    /// Represents an exit case
    Exit {
        /// Type of exit
        ty: ExitType,
        /// Value returned/thrown (None if void required for thrown types)
        value: Option<Box<Exprent<'a>>>,
    },

    /// Conditional statement with a jump to another block
    Condition {
        /// Left hand side of the condition
        left: Box<Exprent<'a>>,
        /// Right hand side of the condition
        right: Box<Exprent<'a>>,
        /// The actual condition
        ty: ConditionType,
        /// The position to jump to
        jump_index: u16,
    },

    /// Represents an operation between two stack items
    Operation {
        /// The left hand side of the operation
        left: Box<Exprent<'a>>,
        /// The right hand side of the operation (None if the operation is on the thing itself)
        right: Option<Box<Exprent<'a>>>,
        /// The type of operation
        ty: OperationType<'a>,
    },

    /// Represents array access
    Array {
        /// The reference for the array
        reference: Box<Exprent<'a>>,
        /// The stack item representing the index within the array
        index: Box<Exprent<'a>>,
    },

    /// Represents an assignment of the left to the right
    Assignment {
        /// Left-hand side of the assignment
        left: Box<Exprent<'a>>,
        /// Right-hand side of the assignment
        right: Box<Exprent<'a>>,
    },

    /// Represents field access
    Field {
        /// The reference to the field
        field: Fieldref<'a>,
        /// Reference to the object the field is on(None for static fields)
        reference: Option<Box<Exprent<'a>>>,
    },

    /// Represents local variable access
    Local {
        /// The index of the local variable
        index: u16,
        /// The type of the local variable
        ty: LocalVariableType,
    },

    // Switch statements
    Switch {
        key: Box<Exprent<'a>>,
        ty: SwitchType,
    },

    /// Represents the creation of a new object
    New(NewType<'a>),

    /// Represents entering and exiting of monitors for
    /// syncronized blocks
    Monitor {
        /// The type of monitor (enter/exit)
        ty: MonitorType,
        /// The reference being monitored
        value: Box<Exprent<'a>>,
    },
}

#[derive(Debug, Clone)]
pub enum MonitorType {
    Enter,
    Exit,
}

#[derive(Debug, Clone)]
pub enum NewType<'a> {
    Object(Class<'a>),
    /// Array of primitive type values 1 dimension
    Array {
        /// The stack item representing the length of the array
        count: Box<Exprent<'a>>,
        /// The type of the array
        ty: ArrayType,
    },
    RefArray {
        /// The length of each array dimension
        counts: Vec<Exprent<'a>>,
        /// The type of the reference
        class: Class<'a>,
    },
}

#[derive(Debug, Clone)]
pub enum SwitchType {
    Lookup {
        default: i32,
        pairs: Vec<(i32, i32)>,
    },
    Table {
        default: i32,
        low: i32,
        high: i32,
        offsets: Vec<i32>,
    },
}

impl<'a> Exprent<'a> {
    fn category(&self) -> u8 {
        // check if this is actually right
        match self {
            Self::Value(value) => value.category(),
            Self::Operation { right, left, ty } => {
                if let OperationType::Cast(cast_to) = ty {
                    cast_to.category()
                } else if let Some(right) = right {
                    right.category()
                } else {
                    left.category()
                }
            }
            Self::Local { ty, .. } => ty.category(),
            Self::Field { field, .. } => field.descriptor.category(),
            _ => 1,
        }
    }
}

impl Display for Exprent<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Exprent::Monitor {
                ty,
                value: reference,
            } => match ty {
                MonitorType::Enter => write!(f, "syncronized ({}) {{", reference),
                MonitorType::Exit => f.write_char('}'),
            },
            Exprent::Value(value) => value.fmt(f),
            Exprent::Operation { right, ty, left } => {
                if let Some(right) = right {
                    write!(f, "{} {} {}", left, ty, right)
                } else {
                    let pos = ty.position();
                    match pos {
                        Position::After => write!(f, "{}{}", left, ty),
                        Position::Before => write!(f, "{}{}", ty, left),
                    }
                }
            }
            Exprent::Local { index, .. } => write!(f, "var{}", index),
            Exprent::Field { field, reference } => {
                if let Some(reference) = reference {
                    write!(f, "{}.{}", reference, field.name)
                } else {
                    write!(f, "{}.{}", field.class.class, field.name)
                }
            }
            Exprent::Array { reference, index } => write!(f, "{}[{}]", reference, index),

            Exprent::New(ty) => {
                f.write_str("new ")?;
                match ty {
                    NewType::Object(class) => write!(f, "{}()", class.class),
                    NewType::Array { count, ty } => {
                        write!(f, "{}[{}]", ty, count)
                    }
                    NewType::RefArray { counts, class } => {
                        class.class.fmt(f)?;
                        for count in counts {
                            write!(f, "[{}]", count)?;
                        }
                        Ok(())
                    }
                }
            }

            Exprent::Invoke {
                method,
                reference,
                args,
                ..
            } => {
                if let Some(reference) = reference {
                    write!(f, "{}.{}(", reference, method.name)?;
                } else {
                    write!(f, "{}.{}", method.class.class, method.name)?;
                }

                for i in 0..args.len() {
                    args[i].fmt(f)?;

                    if i + 1 != args.len() {
                        f.write_str(", ")?;
                    }
                }

                f.write_char(')')?;
                f.write_char(';')
            }
            Exprent::Exit { ty, value } => {
                if let ExitType::Return = ty {
                    f.write_str("return")?;
                } else {
                    f.write_str("throw")?;
                }

                if let Some(value) = value {
                    f.write_char(' ')?;
                    value.fmt(f)?;
                }
                f.write_char(';')
            }
            Exprent::Condition {
                left,
                right,
                ty,
                jump_index,
            } => {
                write!(
                    f,
                    "if ({} {} {}) {{ GOTO: {} }}",
                    left, ty, right, jump_index
                )
            }
            Exprent::Assignment { left, right } => {
                write!(f, "{} = {};", left, right)
            }

            Exprent::Switch { key, ty } => write!(f, " Switch: {} {:?}", key, ty),
        }
    }
}

/// The type of condition
#[derive(Debug, Clone)]
pub enum ConditionType {
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
}

impl Display for ConditionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            ConditionType::Equal => "==",
            ConditionType::NotEqual => "!=",
            ConditionType::GreaterThan => ">",
            ConditionType::GreaterThanOrEqual => ">=",
            ConditionType::LessThan => "<",
            ConditionType::LessThanOrEqual => "<=",
        })
    }
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
    stms: &mut Vec<Exprent<'a>>,
) -> Result<(), ProcessError> {
    use classfile::inst::Instruction::*;

    match instruction {
        // Swapping
        Swap => stack.swap()?,

        // Popping
        Pop => stack.pop_use(stms)?,
        Pop2 => stack.pop2_use(stms)?,

        // Cast checking
        CheckCast(index) => {
            let class_name: &str = pool
                .get_class_name(*index)
                .ok_or(ProcessError::InvalidClassIndex(*index))?;

            let value = stack.pop_boxed()?;
            stack.push(Exprent::Operation {
                left: value,
                right: None,
                ty: OperationType::Cast(FieldDesc::Object(class_name)),
            });
        }

        // Exception throwing
        AThrow => {
            let value = stack.pop_boxed()?;

            stms.push(Exprent::Exit {
                value: Some(value),
                ty: ExitType::Throw,
            })
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

                // TODO: THIS IS SUPPOSED TO INCLUDE THE OTHER TYPES TOO
                _ => return Err(ProcessError::InvalidConstantIndex(*index)),
            };

            stack.push_value(value);
        }

        // Local variable storing
        AStore(index) | LStore(index) | IStore(index) | DStore(index) | FStore(index) => {
            let index = *index;
            let right = stack.pop_boxed()?;

            let ty = match instruction {
                AStore(_) => LocalVariableType::Reference,
                LStore(_) => LocalVariableType::Long,
                IStore(_) => LocalVariableType::Int,
                DStore(_) => LocalVariableType::Double,
                FStore(_) => LocalVariableType::Float,
                _ => panic!("Unexpected store type"),
            };

            let left = Box::new(Exprent::Local { index, ty });

            stms.push(Exprent::Assignment { left, right })
        }

        // Local variable loading
        ILoad(index) | LLoad(index) | FLoad(index) | DLoad(index) | ALoad(index) => {
            let ty = match instruction {
                ALoad(_) => LocalVariableType::Reference,
                LLoad(_) => LocalVariableType::Long,
                ILoad(_) => LocalVariableType::Int,
                DLoad(_) => LocalVariableType::Double,
                FLoad(_) => LocalVariableType::Float,
                _ => panic!("Unexpected store type"),
            };

            stack.push(Exprent::Local { index: *index, ty })
        }

        // Function invokes
        InvokeSpecial(index)
        | InvokeVirtual(index)
        | InvokeInterface(index)
        | InvokeDynamic(index)
        | InvokeStatic(index) => {
            let ty = match instruction {
                InvokeStatic(_) => InvokeType::Static,
                InvokeSpecial(_) => InvokeType::Special,
                InvokeVirtual(_) => InvokeType::Virtual,
                InvokeInterface(_) => InvokeType::Interface,
                InvokeDynamic(_) => InvokeType::Dynamic,
                _ => panic!("Wasn't expeting to handle any other instructions here"),
            };

            let method: Methodref = if let InvokeType::Dynamic = ty {
                // TODO: Version check
                let dynamic = pool.get_invokedynamic(*index).unwrap();

                let name_and_type = pool
                    .get_name_and_type(dynamic.name_and_type)
                    .ok_or(ProcessError::InvalidMethodref(dynamic.name_and_type))?;

                let name_and_type: MethodNameAndType<'a> = pool
                    .get_method_name_and_type(name_and_type)
                    .ok_or(ProcessError::InvalidMethodref(dynamic.name_and_type))?;

                Methodref {
                    // Dummy class for invoke dynamic
                    class: Class {
                        class: "Class",
                        packages: vec!["java", "lang"],
                        outer_classes: vec![],
                    },
                    name: name_and_type.name,
                    descriptor: name_and_type.descriptor,
                }
            } else {
                pool.get_methodref(*index)
                    .ok_or(ProcessError::InvalidMethodref(*index))?
            };

            let args_len = method.descriptor.parameters.len();
            let mut args = Vec::with_capacity(args_len);
            for _ in 0..args_len {
                args.push(stack.pop()?);
            }
            args.reverse();

            let reference = if let InvokeType::Dynamic = ty {
                None
            } else {
                Some(stack.pop_boxed()?)
            };

            let is_void = matches!(&method.descriptor.return_type, FieldDesc::Void);

            let expr = Exprent::Invoke {
                args,
                method,
                reference,
                ty,
            };

            // Non return types should end up on the stack
            if !is_void {
                stack.push(expr)
            } else {
                stms.push(expr)
            }
        }

        // Storing in fields
        PutField(index) => {
            let field: Fieldref<'a> = pool
                .get_fieldref(*index)
                .ok_or(ProcessError::InvalidFieldref(*index))?;

            let right: Box<Exprent> = stack.pop_boxed()?;
            let reference: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = Box::new(Exprent::Field {
                field,
                reference: Some(reference),
            });

            stms.push(Exprent::Assignment { left, right })
        }
        PutStatic(index) => {
            let field: Fieldref = pool
                .get_fieldref(*index)
                .ok_or(ProcessError::InvalidFieldref(*index))?;

            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = Box::new(Exprent::Field {
                field,
                reference: None,
            });

            stms.push(Exprent::Assignment { left, right })
        }

        // Retreiving from fields
        GetField(index) => {
            let field: Fieldref = pool
                .get_fieldref(*index)
                .ok_or(ProcessError::InvalidFieldref(*index))?;
            let reference: Box<Exprent> = stack.pop_boxed()?;
            stack.push(Exprent::Field {
                field,
                reference: Some(reference),
            })
        }
        GetStatic(index) => {
            let field: Fieldref = pool
                .get_fieldref(*index)
                .ok_or(ProcessError::InvalidFieldref(*index))?;
            stack.push(Exprent::Field {
                field,
                reference: None,
            })
        }

        // Array length access
        ArrayLength => {
            let reference: Box<Exprent> = stack.pop_boxed()?;
            stack.push(Exprent::Operation {
                left: reference,
                right: None,
                ty: OperationType::ArrayLength,
            })
        }

        // Returning
        Return => stms.push(Exprent::Exit {
            value: None,
            ty: ExitType::Return,
        }),
        AReturn | DReturn | FReturn | IReturn | LReturn => {
            let value: Box<Exprent> = stack.pop_boxed()?;
            stms.push(Exprent::Exit {
                value: Some(value),
                ty: ExitType::Return,
            })
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
            let index: Box<Exprent> = stack.pop_boxed()?;
            let reference: Box<Exprent> = stack.pop_boxed()?;
            stack.push(Exprent::Array { reference, index })
        }

        // Array storing
        IAStore | LAStore | DAStore | FAStore | SAStore | CAStore | BAStore | AAStore => {
            let index: Box<Exprent> = stack.pop_boxed()?;
            let reference: Box<Exprent> = stack.pop_boxed()?;

            let left: Box<Exprent> = Box::new(Exprent::Array { reference, index });
            let right: Box<Exprent> = stack.pop_boxed()?;

            stms.push(Exprent::Assignment { left, right })
        }

        // Adding
        IAdd | LAdd | FAdd | DAdd => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = stack.pop_boxed()?;
            stack.push(Exprent::Operation {
                left,
                right: Some(right),
                ty: OperationType::Add,
            })
        }

        // Subtracting
        ISub | LSub | FSub | DSub => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = stack.pop_boxed()?;
            stack.push(Exprent::Operation {
                left,
                right: Some(right),
                ty: OperationType::Subtract,
            })
        }

        // Dividing
        IDiv | LDiv | FDiv | DDiv => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = stack.pop_boxed()?;
            stack.push(Exprent::Operation {
                left,
                right: Some(right),
                ty: OperationType::Divide,
            })
        }

        // Multiplying
        IMul | LMul | FMul | DMul => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = stack.pop_boxed()?;
            stack.push(Exprent::Operation {
                left,
                right: Some(right),
                ty: OperationType::Muliply,
            })
        }

        // Remainder
        IRem | LRem | FRem | DRem => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = stack.pop_boxed()?;
            stack.push(Exprent::Operation {
                left,
                right: Some(right),
                ty: OperationType::Remainder,
            })
        }

        // Bitwise And
        IAnd | LAnd => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = stack.pop_boxed()?;
            stack.push(Exprent::Operation {
                left,
                right: Some(right),
                ty: OperationType::BitwiseAnd,
            })
        }

        // | Bitwise OR operation
        LOr | IOr => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = stack.pop_boxed()?;
            stack.push(Exprent::Operation {
                left,
                right: Some(right),
                ty: OperationType::BitwiseOr,
            })
        }

        // ^ XOR operation
        LXOr | IXOr => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = stack.pop_boxed()?;
            stack.push(Exprent::Operation {
                left,
                right: Some(right),
                ty: OperationType::Xor,
            })
        }

        // << Bitwise shift left operation
        IShL | LShL => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = stack.pop_boxed()?;
            stack.push(Exprent::Operation {
                left,
                right: Some(right),
                ty: OperationType::BitwiseShl,
            })
        }

        // >> Bitwise shift right operation
        IShR | LShR => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = stack.pop_boxed()?;
            stack.push(Exprent::Operation {
                left,
                right: Some(right),
                ty: OperationType::BitwiseShr,
            })
        }

        // >>> Logical shift right operation
        IUShR | LUShR => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = stack.pop_boxed()?;
            stack.push(Exprent::Operation {
                left,
                right: Some(right),
                ty: OperationType::LogicalShr,
            })
        }

        // < Less than comparison (float, double)
        FCmpL | DCmpL => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = stack.pop_boxed()?;
            stack.push(Exprent::Operation {
                left,
                right: Some(right),
                ty: OperationType::CompareLess,
            })
        }

        // > Greater than comparison (float, double)
        FCmpG | DCmpG => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = stack.pop_boxed()?;
            stack.push(Exprent::Operation {
                left,
                right: Some(right),
                ty: OperationType::CompareGreater,
            })
        }

        // Negating
        INeg | LNeg | FNeg | DNeg => {
            let value = stack.pop_boxed()?;

            stack.push(Exprent::Operation {
                left: value,
                right: None,
                ty: OperationType::Negate,
            });
        }

        // Int casting
        L2i | D2i | F2i => {
            let value = stack.pop_boxed()?;
            stack.push(Exprent::Operation {
                left: value,
                right: None,
                ty: OperationType::Cast(FieldDesc::Int),
            });
        }

        // Long casting
        I2l | F2l | D2l => {
            let value = stack.pop_boxed()?;
            stack.push(Exprent::Operation {
                left: value,
                right: None,
                ty: OperationType::Cast(FieldDesc::Long),
            });
        }

        // Float casting
        D2f | L2f | I2f => {
            let value = stack.pop_boxed()?;
            stack.push(Exprent::Operation {
                left: value,
                right: None,
                ty: OperationType::Cast(FieldDesc::Float),
            });
        }

        // Double casting
        I2d | L2d | F2d => {
            let value = stack.pop_boxed()?;

            stack.push(Exprent::Operation {
                left: value,
                right: None,
                ty: OperationType::Cast(FieldDesc::Double),
            });
        }

        I2s => {
            let value = stack.pop_boxed()?;
            stack.push(Exprent::Operation {
                left: value,
                right: None,
                ty: OperationType::Cast(FieldDesc::Short),
            });
        }
        I2c => {
            let value = stack.pop_boxed()?;
            stack.push(Exprent::Operation {
                left: value,
                right: None,
                ty: OperationType::Cast(FieldDesc::Char),
            });
        }
        I2b => {
            let value = stack.pop_boxed()?;
            stack.push(Exprent::Operation {
                left: value,
                right: None,
                ty: OperationType::Cast(FieldDesc::Byte),
            });
        }

        // Switches
        LookupSwitch { default, pairs } => {
            let key: Box<Exprent> = stack.pop_boxed()?;

            stms.push(Exprent::Switch {
                key,
                ty: SwitchType::Lookup {
                    default: *default,
                    pairs: pairs.clone(),
                },
            })
        }
        TableSwitch {
            default,
            low,
            high,
            offsets,
        } => {
            let key: Box<Exprent> = stack.pop_boxed()?;
            stms.push(Exprent::Switch {
                key,
                ty: SwitchType::Table {
                    default: *default,
                    low: *low,
                    high: *high,
                    offsets: offsets.clone(),
                },
            })
        }

        MonitorEnter => {
            let value = stack.pop_boxed()?;
            stms.push(Exprent::Monitor {
                ty: MonitorType::Enter,
                value,
            })
        }

        MonitorExit => {
            let value = stack.pop_boxed()?;
            stms.push(Exprent::Monitor {
                ty: MonitorType::Exit,
                value,
            })
        }

        // New instance creation
        New(index) => {
            let class_name: &str = pool
                .get_class_name(*index)
                .ok_or(ProcessError::InvalidClassIndex(*index))?;
            let class: Class =
                Class::try_parse(class_name).ok_or(ProcessError::InvalidClassName)?;
            stack.push(Exprent::New(NewType::Object(class)));
        }

        // Array creation
        NewArray(ty) => {
            let count: Box<Exprent> = stack.pop_boxed()?;
            stack.push(Exprent::New(NewType::Array { count, ty: *ty }));
        }
        ANewArray(index) => {
            let class_name: &str = pool
                .get_class_name(*index)
                .ok_or(ProcessError::InvalidClassIndex(*index))?;
            let class: Class =
                Class::try_parse(class_name).ok_or(ProcessError::InvalidClassName)?;
            let count: Exprent = stack.pop()?;
            stack.push(Exprent::New(NewType::RefArray {
                counts: vec![count],
                class,
            }));
        }
        MultiANewArray { dimensions, index } => {
            let class_name: &str = pool
                .get_class_name(*index)
                .ok_or(ProcessError::InvalidClassIndex(*index))?;
            let class: Class =
                Class::try_parse(class_name).ok_or(ProcessError::InvalidClassName)?;
            let mut counts: Vec<Exprent> = Vec::with_capacity(*dimensions as usize);
            for _ in 0..*dimensions {
                counts.push(stack.pop()?);
            }
            counts.reverse();
            stack.push(Exprent::New(NewType::RefArray { counts, class }));
        }

        // Instance checking
        InstanceOf(index) => {
            let class_name: &str = pool
                .get_class_name(*index)
                .ok_or(ProcessError::InvalidClassIndex(*index))?;
            let class: Class =
                Class::try_parse(class_name).ok_or(ProcessError::InvalidClassName)?;

            let left: Box<Exprent> = stack.pop_boxed()?;
            let right = Box::new(Exprent::Value(Value::Object(class)));

            stms.push(Exprent::Operation {
                left,
                right: Some(right),
                ty: OperationType::InstanceOf,
            })
        }

        // Incrementing
        IInc { index, value } => {
            let left = Box::new(Exprent::Local {
                index: *index,
                ty: LocalVariableType::Int,
            });
            let right = Box::new(Exprent::Value(Value::Integer(*value as i32)));

            stms.push(Exprent::Operation {
                left,
                right: Some(right),
                ty: OperationType::Increment,
            })
        }

        // Reference compare
        IfACmpEq(index) => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = stack.pop_boxed()?;
            stms.push(Exprent::Condition {
                right,
                left,
                ty: ConditionType::Equal,
                jump_index: *index,
            })
        }
        IfACmpNe(index) => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = stack.pop_boxed()?;
            stms.push(Exprent::Condition {
                right,
                left,
                ty: ConditionType::NotEqual,
                jump_index: *index,
            })
        }

        // Int compare
        IfICmpEq(index) => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = stack.pop_boxed()?;
            stms.push(Exprent::Condition {
                right,
                left,
                ty: ConditionType::Equal,
                jump_index: *index,
            })
        }
        IfICmpNe(index) => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = stack.pop_boxed()?;
            stms.push(Exprent::Condition {
                right,
                left,
                ty: ConditionType::NotEqual,
                jump_index: *index,
            })
        }
        IfICmpLt(index) => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = stack.pop_boxed()?;
            stms.push(Exprent::Condition {
                right,
                left,
                ty: ConditionType::LessThan,
                jump_index: *index,
            })
        }
        IfICmpLe(index) => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = stack.pop_boxed()?;
            stms.push(Exprent::Condition {
                right,
                left,
                ty: ConditionType::LessThanOrEqual,
                jump_index: *index,
            })
        }
        IfICmpGt(index) => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = stack.pop_boxed()?;
            stms.push(Exprent::Condition {
                right,
                left,
                ty: ConditionType::GreaterThan,
                jump_index: *index,
            })
        }

        IfICmpGe(index) => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = stack.pop_boxed()?;
            stms.push(Exprent::Condition {
                right,
                left,
                ty: ConditionType::GreaterThanOrEqual,
                jump_index: *index,
            })
        }

        // Long compare
        LCmp => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = stack.pop_boxed()?;
            stack.push(Exprent::Operation {
                left,
                right: Some(right),
                ty: OperationType::SignedCompare,
            })
        }

        // Null compare
        IfNull(index) => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = Box::new(Exprent::Value(Value::Null));
            stms.push(Exprent::Condition {
                right,
                left,
                ty: ConditionType::Equal,
                jump_index: *index,
            })
        }
        IfNonNull(index) => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = Box::new(Exprent::Value(Value::Null));
            stms.push(Exprent::Condition {
                right,
                left,
                ty: ConditionType::NotEqual,
                jump_index: *index,
            })
        }

        // Int zero compare
        IfEq(index) => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = Box::new(Exprent::Value(Value::Integer(0)));
            stms.push(Exprent::Condition {
                right,
                left,
                ty: ConditionType::Equal,
                jump_index: *index,
            })
        }
        IfNe(index) => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = Box::new(Exprent::Value(Value::Integer(0)));
            stms.push(Exprent::Condition {
                right,
                left,
                ty: ConditionType::NotEqual,
                jump_index: *index,
            })
        }
        IfLt(index) => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = Box::new(Exprent::Value(Value::Integer(0)));
            stms.push(Exprent::Condition {
                right,
                left,
                ty: ConditionType::LessThan,
                jump_index: *index,
            })
        }
        IfLe(index) => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = Box::new(Exprent::Value(Value::Integer(0)));
            stms.push(Exprent::Condition {
                right,
                left,
                ty: ConditionType::LessThanOrEqual,
                jump_index: *index,
            })
        }
        IfGe(index) => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = Box::new(Exprent::Value(Value::Integer(0)));
            stms.push(Exprent::Condition {
                right,
                left,
                ty: ConditionType::GreaterThanOrEqual,
                jump_index: *index,
            })
        }
        IfGt(index) => {
            let right: Box<Exprent> = stack.pop_boxed()?;
            let left: Box<Exprent> = Box::new(Exprent::Value(Value::Integer(0)));
            stms.push(Exprent::Condition {
                right,
                left,
                ty: ConditionType::GreaterThan,
                jump_index: *index,
            })
        }

        JSr(_) | Ret(_) => {}
        Goto(_) | Nop => {}
    }

    Ok(())
}
