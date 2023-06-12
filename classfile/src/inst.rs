use nom::{
    bytes::complete::take,
    combinator::{fail, map},
    multi::count,
    number::complete::{be_i16, be_i32, be_u16, i8, u8},
    sequence::tuple,
    IResult,
};

use crate::constant_pool::PoolIndex;

pub type Index = u16;
pub type BranchIndex = u16;

#[derive(Debug, Clone, Copy)]
pub enum ArrayType {
    Boolean = 4,
    Char = 5,
    Float = 6,
    Double = 7,
    Byte = 8,
    Short = 9,
    Int = 10,
    Long = 11,
}

fn array_type(input: &[u8]) -> IResult<&[u8], ArrayType> {
    let (input, ty) = u8(input)?;

    let array_type = match ty {
        4 => ArrayType::Boolean,
        5 => ArrayType::Char,
        6 => ArrayType::Float,
        7 => ArrayType::Double,
        8 => ArrayType::Byte,
        9 => ArrayType::Short,
        10 => ArrayType::Int,
        11 => ArrayType::Long,
        _ => return fail(input),
    };

    Ok((input, array_type))
}

fn maybe_wide<O>(wide: bool, mut out: O) -> impl FnMut(&[u8]) -> IResult<&[u8], Instruction>
where
    O: FnMut(u16) -> Instruction + Copy,
{
    move |input| {
        if wide {
            map(be_u16, out)(input)
        } else {
            map(u8, |value| out(value as u16))(input)
        }
    }
}

fn table_switch(input: &[u8], pos: i32) -> IResult<&[u8], Instruction> {
    let padding = (1 + ((pos - 1) / 4)) * 4 - pos;

    let (input, _) = take(padding as usize)(input)?;
    let (input, default) = be_i32(input)?;
    let default = pos + default;
    let (input, low) = be_i32(input)?;
    let (input, high) = be_i32(input)?;
    // TODO: Bounds checking for high low
    let (input, offsets) = count(be_i32, (high - low) as usize)(input)?;

    let offsets = offsets.into_iter().map(|offset| (pos + offset)).collect();

    Ok((
        input,
        Instruction::TableSwitch(TableSwitchData {
            default,
            low,
            high,
            offsets,
        }),
    ))
}

#[derive(Debug, Clone)]
pub struct TableSwitchData {
    pub default: i32,
    pub low: i32,
    pub high: i32,
    pub offsets: Vec<i32>,
}

fn lookup_switch(input: &[u8], pos: i32) -> IResult<&[u8], Instruction> {
    let padding = (1 + ((pos - 1) / 4)) * 4 - pos;
    let (input, _) = take(padding as usize)(input)?;
    let (input, default) = be_i32(input)?;
    let default = pos + default;
    let (input, length) = be_i32(input)?;
    let (input, pairs) = count(tuple((be_i32, be_i32)), length as usize)(input)?;

    let pairs = pairs.into_iter().map(|(a, b)| (a, pos + b)).collect();

    Ok((
        input,
        Instruction::LookupSwitch(LookupSwitchData { default, pairs }),
    ))
}

#[derive(Debug, Clone)]
pub struct LookupSwitchData {
    pub default: i32,
    pub pairs: Vec<(i32, i32)>,
}

pub fn instruction(input: &[u8], wide: bool, pos: i32) -> IResult<&[u8], Instruction> {
    use Instruction::*;

    let (input, code) = u8(input)?;
    let instr = match code {
        0x0 => Nop,
        0x1 => AConstNull,

        0x2 => IConst(-1),
        0x3 => IConst(0),
        0x4 => IConst(1),
        0x5 => IConst(2),
        0x6 => IConst(3),
        0x7 => IConst(4),
        0x8 => IConst(5),

        0x9 => LConst(0),
        0xa => LConst(1),

        0xb => FConst(0.0),
        0xc => FConst(1.0),
        0xd => FConst(2.0),
        0xe => DConst(0.0),
        0xf => DConst(1.0),

        0x10 => return map(i8, BIPush)(input),
        0x11 => return map(be_i16, SIPush)(input),
        0x12 => return map(u8, |value| LoadConst(value as u16))(input),
        0x13 | 0x14 => return map(be_u16, LoadConst)(input),
        0x15 => return maybe_wide(wide, ILoad)(input),
        0x16 => return maybe_wide(wide, LLoad)(input),
        0x17 => return maybe_wide(wide, FLoad)(input),
        0x18 => return maybe_wide(wide, DLoad)(input),
        0x19 => return maybe_wide(wide, ALoad)(input),

        0x1a..=0x21 => ILoad((code - 0x1a) as u16),
        0x22..=0x25 => FLoad((code - 0x22) as u16),
        0x26..=0x29 => DLoad((code - 0x26) as u16),
        0x2a..=0x2d => ALoad((code - 0x2a) as u16),

        0x2e => IALoad,
        0x2f => LALoad,
        0x30 => FALoad,
        0x31 => DALoad,
        0x32 => AALoad,
        0x33 => BALoad,
        0x34 => CALoad,
        0x35 => SALoad,

        0x36 => return maybe_wide(wide, IStore)(input),
        0x37 => return maybe_wide(wide, LStore)(input),
        0x38 => return maybe_wide(wide, FStore)(input),
        0x39 => return maybe_wide(wide, DStore)(input),
        0x3a => return maybe_wide(wide, AStore)(input),

        0x3b..=0x3e => IStore((code - 0x3b) as u16),
        0x3f..=0x42 => LStore((code - 0x3f) as u16),
        0x43..=0x46 => FStore((code - 0x43) as u16),
        0x47..=0x4a => DStore((code - 0x47) as u16),
        0x4b..=0x4e => AStore((code - 0x4b) as u16),

        0x4f => IAStore,
        0x50 => LAStore,
        0x51 => FAStore,
        0x52 => DAStore,
        0x53 => AAStore,
        0x54 => BAStore,
        0x55 => CAStore,
        0x56 => SAStore,
        0x57 => Pop,
        0x58 => Pop2,
        0x59 => Dup,
        0x5a => DupX1,
        0x5b => DupX2,
        0x5c => Dup2,
        0x5d => Dup2X1,
        0x5e => Dup2X2,
        0x5f => Swap,
        0x60 => IAdd,
        0x61 => LAdd,
        0x62 => FAdd,
        0x63 => DAdd,
        0x64 => ISub,
        0x65 => LSub,
        0x66 => FSub,
        0x67 => DSub,
        0x68 => IMul,
        0x69 => LMul,
        0x6a => FMul,
        0x6b => DMul,
        0x6c => IDiv,
        0x6d => LDiv,
        0x6e => FDiv,
        0x6f => DDiv,
        0x70 => IRem,
        0x71 => LRem,
        0x72 => FRem,
        0x73 => DRem,
        0x74 => INeg,
        0x75 => LNeg,
        0x76 => FNeg,
        0x77 => DNeg,
        0x78 => IShL,
        0x79 => LShL,
        0x7a => IShR,
        0x7b => LShR,
        0x7c => IUShR,
        0x7d => LUShR,
        0x7e => IAnd,
        0x7f => LAnd,
        0x80 => IOr,
        0x81 => LOr,
        0x82 => IXOr,
        0x83 => LXOr,
        0x84 => {
            return if wide {
                map(tuple((be_u16, be_i16)), |(index, value)| IInc {
                    index,
                    value,
                })(input)
            } else {
                map(tuple((u8, u8)), |(index, value)| IInc {
                    index: index as u16,
                    value: value as i16,
                })(input)
            }
        }
        0x85 => I2l,
        0x86 => I2f,
        0x87 => I2d,
        0x88 => L2i,
        0x89 => L2f,
        0x8a => L2d,
        0x8b => F2i,
        0x8c => F2l,
        0x8d => F2d,
        0x8e => D2i,
        0x8f => D2l,
        0x90 => D2f,
        0x91 => I2b,
        0x92 => I2c,
        0x93 => I2s,
        0x94 => LCmp,
        0x95 => FCmpL,
        0x96 => FCmpG,
        0x97 => DCmpL,
        0x98 => DCmpG,

        0x99 => return map(be_i16, |value| IfEq((pos + (value as i32)) as u16))(input),
        0x9a => return map(be_i16, |value| IfNe((pos + (value as i32)) as u16))(input),
        0x9b => return map(be_i16, |value| IfLt((pos + (value as i32)) as u16))(input),
        0x9c => return map(be_i16, |value| IfGe((pos + (value as i32)) as u16))(input),
        0x9d => return map(be_i16, |value| IfGt((pos + (value as i32)) as u16))(input),
        0x9e => return map(be_i16, |value| IfLe((pos + (value as i32)) as u16))(input),
        0x9f => return map(be_i16, |value| IfICmpEq((pos + (value as i32)) as u16))(input),
        0xa0 => return map(be_i16, |value| IfICmpNe((pos + (value as i32)) as u16))(input),
        0xa1 => return map(be_i16, |value| IfICmpLt((pos + (value as i32)) as u16))(input),
        0xa2 => return map(be_i16, |value| IfICmpGe((pos + (value as i32)) as u16))(input),
        0xa3 => return map(be_i16, |value| IfICmpGt((pos + (value as i32)) as u16))(input),
        0xa4 => return map(be_i16, |value| IfICmpLe((pos + (value as i32)) as u16))(input),
        0xa5 => return map(be_i16, |value| IfACmpEq((pos + (value as i32)) as u16))(input),
        0xa6 => return map(be_i16, |value| IfACmpNe((pos + (value as i32)) as u16))(input),
        0xa7 => return map(be_i16, |value| Goto((pos + (value as i32)) as u16))(input),
        0xa8 => return map(be_i16, |value| JSr((pos + (value as i32)) as u16))(input),

        0xa9 => {
            return if wide {
                map(be_u16, Ret)(input)
            } else {
                map(u8, |value| Ret(value as u16))(input)
            }
        }

        0xaa => return table_switch(input, pos),
        0xab => return lookup_switch(input, pos),

        0xac => IReturn,
        0xad => LReturn,
        0xae => FReturn,
        0xaf => DReturn,
        0xb0 => AReturn,
        0xb1 => Return,
        0xb2 => return map(be_u16, GetStatic)(input),
        0xb3 => return map(be_u16, PutStatic)(input),
        0xb4 => return map(be_u16, GetField)(input),
        0xb5 => return map(be_u16, PutField)(input),
        0xb6 => return map(be_u16, InvokeVirtual)(input),
        0xb7 => return map(be_u16, InvokeSpecial)(input),
        0xb8 => return map(be_u16, InvokeStatic)(input),

        0xb9 => return map(tuple((be_u16, be_u16)), |(index, _)| InvokeInterface(index))(input),
        0xba => return map(tuple((be_u16, be_u16)), |(index, _)| InvokeDynamic(index))(input),

        0xbb => return map(be_u16, New)(input),
        0xbc => return map(array_type, NewArray)(input),
        0xbd => return map(be_u16, ANewArray)(input),
        0xbe => ArrayLength,
        0xbf => AThrow,
        0xc0 => return map(be_u16, CheckCast)(input),
        0xc1 => return map(be_u16, InstanceOf)(input),
        0xc2 => MonitorEnter,
        0xc3 => MonitorExit,
        // Read the next instruction using WIDE
        0xc4 => return instruction(input, true, pos + 1),

        0xc5 => {
            return map(tuple((be_u16, u8)), |(index, dimensions)| MultiANewArray {
                index,
                dimensions,
            })(input)
        }

        0xc6 => return map(be_i16, |value| IfNull((pos + (value as i32)) as u16))(input),
        0xc7 => return map(be_i16, |value| IfNonNull((pos + (value as i32)) as u16))(input),
        0xc8 => return map(be_i32, |value| Goto((pos + value) as u16))(input),
        0xc9 => return map(be_i32, |value| JSr((pos + value) as u16))(input),

        // TODO: Properly handle
        _ => return fail(input),
    };
    Ok((input, instr))
}

/// Local variable indexes are stored as u16 rather than
/// u8 in order to account for wide instructions
type LocalVariableIndex = u16;

#[derive(Debug, Clone)]
pub struct IIncData {
    /// The index of the local variable
    pub index: LocalVariableIndex,
    /// The value to add to the variable
    pub value: i16,
}

#[derive(Debug, Clone)]
pub struct MultiANewArrayData {
    /// The index of the array object type in the constant pool
    pub index: PoolIndex,
    /// The number of dimensions the array has
    pub dimension: u8,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    /// Load reference from array
    /// [..., arrayref, index → ..., value]
    AALoad,
    /// Store into reference array
    /// [..., arrayref, index, value → ...]
    AAStore,
    /// Push null
    /// [... → ..., null]
    AConstNull,
    /// Load reference from local variable
    /// [... → ..., objectref]
    ALoad(LocalVariableIndex),
    /// Create new array of reference  
    /// [..., count → ..., arrayref]
    ANewArray(PoolIndex),
    /// Return reference from method
    /// [..., objectref → [empty]]
    AReturn,
    /// Get length of array
    /// [..., arrayref → ..., length]
    ArrayLength,
    /// Store reference into local variable
    /// [..., objectref → ...]
    AStore(LocalVariableIndex),
    /// Throw exception or error
    /// [..., objectref → objectref]
    AThrow,
    /// Load byte or boolean from array
    /// [..., arrayref, index → ..., value]
    BALoad,
    /// Store into byte or boolean array
    /// [..., arrayref, index, value → ...]
    BAStore,
    /// Push byte
    /// [... → ..., value]
    BIPush(i8),
    /// Load char from array
    /// [..., arrayref, index → ..., value]
    CALoad,
    /// Store into char array
    /// [..., arrayref, index, value → ...]
    CAStore,
    /// Check whether object is of given type
    /// [..., objectref → ..., objectref]
    CheckCast(PoolIndex),
    /// Convert double to float
    /// [..., value → ..., result]
    D2f,
    /// Convert double to int
    /// [..., value → ..., result]
    D2i,
    /// Convert double to long
    /// [..., value → ..., result]
    D2l,
    /// Add double
    /// [..., value1, value2 → ..., result]
    DAdd,
    /// Load double from array
    /// [..., arrayref, index → ..., value]
    DALoad,
    /// Store into double array
    /// [..., arrayref, index, value → ...]
    DAStore,
    /// Compare double
    /// [..., value1, value2 → ..., result]
    DCmpG,
    /// Compare double
    /// [..., value1, value2 → ..., result]
    DCmpL,
    /// Push double
    /// [... → ..., <d>]
    DConst(f64),
    /// Divide double  
    /// [..., value1, value2 → ..., result]
    DDiv,
    /// Load double from local variable
    /// [... → ..., value]
    DLoad(LocalVariableIndex),
    /// Multiply double
    /// [..., value1, value2 → ..., result]
    DMul,
    /// Negate double
    /// [..., value → ..., result]
    DNeg,
    /// Remainder double
    /// [..., value1, value2 → ..., result]
    DRem,
    /// Return double from method
    /// [..., value → [empty]]
    DReturn,
    /// Store double into local variable
    /// [..., value → ...]
    DStore(LocalVariableIndex),
    /// Subtract double
    /// [..., value1, value2 → ..., result]
    DSub,
    /// Duplicate the top operand stack value
    /// [..., value → ..., value, value]
    Dup,
    /// Duplicate the top operand stack value and insert two values down
    /// [..., value2, value1 → ..., value1, value2, value1]
    DupX1,
    /// Duplicate the top operand stack value and insert two or three values down
    /// 1: [..., value3, value2, value1 → ..., value1, value3, value2, value1]
    /// 2: [..., value2, value1 → ..., ..., value1, value2, value1]
    DupX2,
    /// Duplicate the top one or two operand stack values
    /// 1. [..., value2, value1 → ..., value2, value1, value2, value1]
    /// 2. [..., value → ..., value, value]
    Dup2,
    /// Duplicate the top one or two operand stack values and insert two or three values down
    /// 1. [..., value3, value2, value1 → ..., value2, value1, value3, value2, value1]
    /// 2. [..., value2, value1 → ..., value1, value2, value1]
    Dup2X1,
    /// Duplicate the top one or two operand stack values and insert two, three, or four values down
    /// 1. [..., value4, value3, value2, value1 → ..., value2, value1, value4, value3, value2, value1]
    /// 2. [..., value3, value2, value1 → ..., value1, value3, value2, value1]
    /// 3. [..., value3, value2, value1 → ..., value2, value1, value3, value2, value1]
    /// 4. [..., value2, value1 → ..., value1, value2, value1]
    Dup2X2,
    /// Convert float to double
    /// [..., value → ..., result]
    F2d,
    /// Convert float to int
    /// [..., value → ..., result]
    F2i,
    /// Convert float to long
    /// [..., value → ..., result]
    F2l,
    /// Add float
    /// [..., value1, value2 → ..., result]
    FAdd,
    /// Load float from array
    /// [..., arrayref, index → ..., value]
    FALoad,
    /// Store into float array
    /// [..., arrayref, index, value → ...]
    FAStore,
    /// Compare float
    /// [..., value1, value2 → ..., result]
    FCmpL,
    /// Compare float
    /// [..., value1, value2 → ..., result]
    FCmpG,
    /// Push float
    /// [... → ..., <f>]
    FConst(f32),
    /// Divide float
    /// [..., value1, value2 → ..., result]
    FDiv,
    /// Load float from local variable
    /// [... → ..., value]
    FLoad(LocalVariableIndex),
    /// Multiply float
    /// [..., value1, value2 → ..., result]
    FMul,
    /// Negate float
    /// [..., value → ..., result]
    FNeg,
    /// Remainder float
    /// [..., value1, value2 → ..., result]
    FRem,
    /// Return float from method
    /// [..., value → [empty]]
    FReturn,
    /// Store float into local variable
    /// [..., value → ...]
    FStore(LocalVariableIndex),
    /// Subtract float
    /// [..., value1, value2 → ..., result]
    FSub,
    /// Fetch field from object
    /// [..., objectref → ..., value]
    GetField(PoolIndex),
    /// Get static field from class
    /// [..., → ..., value]
    GetStatic(PoolIndex),
    /// Branch always
    /// [... → ...]
    Goto(BranchIndex),
    /// Convert int to byte
    /// [..., value → ..., result]
    I2b,
    /// Convert int to char
    /// [..., value → ..., result]
    I2c,
    /// Convert int to double
    /// [..., value → ..., result]
    I2d,
    /// Convert int to float
    /// [..., value → ..., result]
    I2f,
    /// Convert int to long
    /// [..., value → ..., result]
    I2l,
    /// Convert int to short
    /// [..., value → ..., result]
    I2s,
    /// Add int
    /// [..., value1, value2 → ..., result]
    IAdd,
    /// Load int from array
    /// [..., arrayref, index → ..., value]
    IALoad,
    /// Boolean AND int
    /// [..., value1, value2 → ..., result]
    IAnd,
    /// Store into int array
    /// [..., arrayref, index, value → ...]
    IAStore,
    /// Push int constant
    /// [... → ..., <i>]
    IConst(i32),
    /// Divide int
    /// [..., value1, value2 → ..., result]
    IDiv,
    /// Branch if reference comparison succeeds
    /// [..., value1, value2 → ...]
    IfACmpEq(BranchIndex),
    /// Branch if reference comparison succeeds
    /// [..., value1, value2 → ...]
    IfACmpNe(BranchIndex),
    /// Branch if int comparison succeeds
    /// [..., value1, value2 → ...]
    IfICmpEq(BranchIndex),
    /// Branch if int comparison succeeds
    /// [..., value1, value2 → ...]
    IfICmpNe(BranchIndex),
    /// Branch if int comparison succeeds
    /// [..., value1, value2 → ...]
    IfICmpLt(BranchIndex),
    /// Branch if int comparison succeeds
    /// [..., value1, value2 → ...]
    IfICmpGe(BranchIndex),
    /// Branch if int comparison succeeds
    /// [..., value1, value2 → ...]
    IfICmpGt(BranchIndex),
    /// Branch if int comparison succeeds
    /// [..., value1, value2 → ...]
    IfICmpLe(BranchIndex),
    /// Branch if int comparison with zero succeeds
    /// [..., value → ...]
    IfEq(BranchIndex),
    /// Branch if int comparison with zero succeeds
    /// [..., value → ...]
    IfNe(BranchIndex),
    /// Branch if int comparison with zero succeeds
    /// [..., value → ...]
    IfLt(BranchIndex),
    /// Branch if int comparison with zero succeeds
    /// [..., value → ...]
    IfGe(BranchIndex),
    /// Branch if int comparison with zero succeeds
    /// [..., value → ...]
    IfGt(BranchIndex),
    /// Branch if int comparison with zero succeeds
    /// [..., value → ...]
    IfLe(BranchIndex),
    /// Branch if reference not null
    /// [..., value → ...]
    IfNonNull(BranchIndex),
    /// Branch if reference is null
    /// [..., value → ...]
    IfNull(BranchIndex),
    /// Increment local variable by constant
    /// [... → ...]
    IInc(IIncData),
    /// Load int from local variable
    /// [... → ..., value]
    ILoad(LocalVariableIndex),
    /// Multiply int
    /// [..., value1, value2 → ..., result]
    IMul,
    /// Negate int
    /// [..., value → ..., result]
    INeg,
    /// Determine if object is of given type
    /// [..., objectref → ..., result]
    InstanceOf(PoolIndex),
    /// Invoke dynamic method
    /// [..., [arg1, [arg2 ...]] → ...]
    InvokeDynamic(PoolIndex),
    /// Invoke interface method
    /// [..., objectref, [arg1, [arg2 ...]] → ...]
    InvokeInterface(PoolIndex),
    /// Invoke instance method; special handling for superclass, private, and instance initialization method invocations
    /// [..., objectref, [arg1, [arg2 ...]] → ...]
    InvokeSpecial(PoolIndex),
    /// Invoke a class (static) method
    /// [..., [arg1, [arg2 ...]] → ...]
    InvokeStatic(PoolIndex),
    /// Invoke instance method; dispatch based on class
    /// [..., objectref, [arg1, [arg2 ...]] → ...]
    InvokeVirtual(PoolIndex),
    /// Boolean OR int
    /// [..., value1, value2 → ..., result]
    IOr,
    /// Remainder int
    /// [..., value1, value2 → ..., result]
    IRem,
    /// Return int from method
    /// [..., value → [empty]]
    IReturn,
    /// Shift left int
    /// [..., value1, value2 → ..., result]
    IShL,
    /// Arithmetic shift right int
    /// [..., value1, value2 → ..., result]
    IShR,
    /// Store int into local variable
    /// [..., value → ...]
    IStore(LocalVariableIndex),
    /// Subtract int
    /// [..., value1, value2 → ..., result]
    ISub,
    /// Logical shift right int
    /// [..., value1, value2 → ..., result]
    IUShR,
    /// Boolean XOR int
    /// [..., value1, value2 → ..., result]
    IXOr,
    /// Jump subroutine
    /// [... → .., address]
    JSr(BranchIndex),
    /// Convert long to double
    /// [..., value → ..., result]
    L2d,
    /// Convert long to float
    /// [..., value → ..., result]
    L2f,
    /// Convert long to int
    /// [..., value → ..., result]
    L2i,
    /// Add long
    /// [..., value1, value2 → ..., result]
    LAdd,
    /// Load long from array
    /// [..., arrayref, index → ..., value]
    LALoad,
    /// Boolean AND long
    /// [..., value1, value2 → ..., result]
    LAnd,
    /// Store into long array
    /// [..., arrayref, index, value → ...]
    LAStore,
    /// Compare long
    /// [..., value1, value2 → ..., result]
    LCmp,
    /// Push long constant
    /// [... → ..., <l>]
    LConst(i64),
    /// Push item from run-time constant pool
    /// [... → ..., value]
    LoadConst(PoolIndex),
    /// Divide long
    /// [..., value1, value2 → ..., result]
    LDiv,
    /// Load long from local variable
    /// [... → ..., value]
    LLoad(LocalVariableIndex),
    /// Multiply long
    /// [..., value1, value2 → ..., result]
    LMul,
    /// Negate long
    /// [..., value → ..., result]
    LNeg,
    /// Access jump table by key match and jump
    /// [..., key → ...]
    LookupSwitch(LookupSwitchData),
    /// Boolean OR long
    /// [..., value1, value2 → ..., result]
    LOr,
    /// Remainder long
    /// [..., value1, value2 → ..., result]
    LRem,
    /// Return long from method
    /// [..., value → [empty]]
    LReturn,
    /// Shift left long
    /// [..., value1, value2 → ..., result]
    LShL,
    /// Arithmetic shift right long
    /// [..., value1, value2 → ..., result]
    LShR,
    /// Store long into local variable
    /// [..., value → ...]
    LStore(LocalVariableIndex),
    /// Subtract long
    /// [..., value1, value2 → ..., result]
    LSub,
    /// Logical shift right long
    /// [..., value1, value2 → ..., result]
    LUShR,
    /// Boolean XOR long
    /// [..., value1, value2 → ..., result]
    LXOr,
    /// Enter monitor for object
    /// [..., objectref → ...]
    MonitorEnter,
    /// Exit monitor for object
    /// [..., objectref → ...]
    MonitorExit,
    /// Create new multidimensional array
    /// [..., count1, [count2, ...] → ..., arrayref]
    MultiANewArray(MultiANewArrayData),
    /// Create new object
    /// [... → ..., objectref]
    New(PoolIndex),
    /// Create new array
    /// [..., count → ..., arrayref]
    NewArray(ArrayType),
    /// Do nothing
    /// [... → ...]
    Nop,
    /// Pop the top operand stack value
    /// [..., value → ...]
    Pop,
    /// Pop the top one or two operand stack values
    /// 1. [..., value2, value1 → ...]
    /// 2. [..., value → ...]
    Pop2,
    /// Set field in object
    /// [..., objectref, value → ...]
    PutField(PoolIndex),
    /// Set static field in class
    /// [..., value → ...]
    PutStatic(PoolIndex),
    /// Return from subroutine
    /// [... → ...]
    Ret(LocalVariableIndex),
    /// Return void from method
    /// [... → [empty]]
    Return,
    /// Load short from array
    /// [..., arrayref, index → ..., value]
    SALoad,
    /// Store into short array
    /// [..., arrayref, index, value → ...]
    SAStore,
    /// Push short
    /// [... → ..., value]
    SIPush(i16),
    /// Swap the top two operand stack values
    /// [..., value2, value1 → ..., value1, value2]
    Swap,
    /// Access jump table by index and jump
    /// [..., index → ...]
    TableSwitch(TableSwitchData),
}
