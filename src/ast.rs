use classfile::{
    attributes::Code,
    constant_pool::{ConstantItem, ConstantPool, PoolIndex},
};
use thiserror::Error;

pub enum AST<'a> {
    /// Constant literals
    Constant(Constant<'a>),
    /// Operation on two different AST items
    Operation(Operation<'a>),
    /// Negated value -1
    Negated(Box<AST<'a>>),
    /// Conditial statement
    Condition(JumpCondition<'a>),
}

pub struct JumpCondition<'a> {
    /// Left hand side of the condition
    left: Box<AST<'a>>,
    /// Right hand side of the condition
    right: Box<AST<'a>>,
    /// The actual condition
    ty: ConditionType,
    /// The position to jump to
    jump_index: u16,
}

pub enum ConditionType {
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Eq,
}

pub struct Operation<'a> {
    left: Box<AST<'a>>,
    ty: OperationType,
    right: Box<AST<'a>>,
}

pub enum Constant<'a> {
    Null,
    String(&'a str),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
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

impl<'a> Constant<'a> {
    pub fn try_from_pool(
        index: PoolIndex,
        pool: &ConstantPool<'a>,
    ) -> Result<Constant<'a>, InstrError> {
        let value = pool.get(index).ok_or(InstrError::MissingConstant(index))?;
        Ok(match value {
            ConstantItem::Integer(value) => Constant::Integer(*value),
            ConstantItem::Float(value) => Constant::Float(*value),
            ConstantItem::Long(value) => Constant::Long(*value),
            ConstantItem::Double(value) => Constant::Double(*value),
            ConstantItem::String(value) => {
                let value = pool
                    .get_utf8(*value)
                    .ok_or(InstrError::UnexpectedConstantType(index))?;
                Constant::String(value)
            }
            _ => return Err(InstrError::UnexpectedConstantType(index)),
        })
    }
}

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

pub fn generate_ast<'a>(code: Code, pool: &ConstantPool<'a>) -> Result<(), InstrError> {
    use classfile::inst::Instruction::*;

    let mut stack: Stack<'a> = Stack::default();
    let mut ast: Vec<AST> = Vec::new();

    for (_pos, instr) in &code.code {
        match instr {
            // Swap the top two stack items
            Swap => {
                stack.swap()?;
            }

            // Push constant onto the stack from the constant pool
            LoadConst(index) => {
                let value = Constant::try_from_pool(*index, pool)?;
                stack.push(AST::Constant(value))
            }

            // Pushing constants
            IConst(value) => stack.push(AST::Constant(Constant::Integer(*value))),
            DConst(value) => stack.push(AST::Constant(Constant::Double(*value))),
            FConst(value) => stack.push(AST::Constant(Constant::Float(*value))),
            LConst(value) => stack.push(AST::Constant(Constant::Long(*value))),
            AConstNull => stack.push(AST::Constant(Constant::Null)),

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

            _ => {}
        }
    }

    Ok(())
}
