use classfile::{
    attributes::Code,
    constant_pool::{ConstantItem, ConstantPool, PoolIndex},
};

pub enum AST<'a> {
    Operation(Operation<'a>),
}

pub struct Operation<'a> {
    left: Box<AST<'a>>,
    ty: OperationType,
    right: Box<AST<'a>>,
}

pub enum Constant<'a> {
    String(&'a str),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
}

#[derive(Debug)]
pub enum InstrError {
    MissingConstant(PoolIndex),
    UnexpectedConstantType(PoolIndex),
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

pub enum StackItem<'a> {
    Constant(Constant<'a>),
}

#[derive(Default)]
pub struct Stack<'a> {
    inner: Vec<StackItem<'a>>,
}

#[derive(Debug)]
pub enum StackError {
    /// Stack was empty
    Empty,
    /// Stack didn't have enough items to pop
    NotEnough {
        /// The length required
        required: usize,
        /// The length remaining
        length: usize,
    },
}

type StackResult<T> = Result<T, StackError>;

impl<'a> Stack<'a> {
    fn push(&mut self, value: StackItem<'a>) {
        self.inner.push(value);
    }

    fn pop(&mut self) -> StackResult<AST> {
        self.inner.pop().ok_or(StackError::Empty)
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
                stack.push(StackItem::Constant(value))
            }

            // Pushing constants
            IConst(value) => stack.push(StackItem::Constant(Constant::Integer(*value))),
            DConst(value) => stack.push(StackItem::Constant(Constant::Double(*value))),
            FConst(value) => stack.push(StackItem::Constant(Constant::Float(*value))),
            LConst(value) => stack.push(StackItem::Constant(Constant::Long(*value))),

            // Multiply operation
            IMul | FMul | DMul | LMul => {
                let left = stack.pop()?;
                let right = stack.pop()?;
                ast.push(AST::Operation(Operation {
                    left: Box::new(left),
                    ty: OperationType::Muliply,
                    right: Box::new(right),
                }))
            }

            _ => {}
        }
    }

    Ok(())
}
