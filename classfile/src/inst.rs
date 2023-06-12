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

#[derive(Debug, Clone)]
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
        Instruction::TableSwitch {
            default,
            low,
            high,
            offsets,
        },
    ))
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

#[derive(Debug, Clone)]
pub enum Instruction {
    SALoad,
    TableSwitch {
        default: i32,
        low: i32,
        high: i32,
        offsets: Vec<i32>,
    },
    Swap,
    SAStore,
    BIPush(i8),
    SIPush(i16),
    NewArray(ArrayType),
    Pop2,
    IConst(i32),
    FConst(f32),
    DConst(f64),
    LConst(i64),
    IAdd,
    FAdd,
    InvokeSpecial(PoolIndex),
    InvokeStatic(PoolIndex),
    InvokeVirtual(PoolIndex),
    InvokeInterface(PoolIndex),
    PutField(PoolIndex),
    GetField(PoolIndex),
    PutStatic(PoolIndex),
    Return,
    Dup,
    DupX1,
    DupX2,
    Dup2,
    Dup2X1,
    Dup2X2,
    Pop,
    DAdd,
    DDiv,
    D2i,
    D2f,
    D2l,
    AReturn,
    CheckCast(PoolIndex),
    F2i,
    AConstNull,
    LoadConst(PoolIndex),
    DCmpL,
    DCmpG,
    ArrayLength,
    AThrow,
    DALoad,
    CALoad,
    BALoad,
    AALoad,
    FALoad,
    DAStore,
    CAStore,
    BAStore,
    AAStore,
    FAStore,
    ANewArray(PoolIndex),
    DMul,
    DNeg,
    DRem,
    DReturn,
    FSub,
    FMul,
    FNeg,
    FRem,
    FReturn,
    FCmpL,
    FCmpG,
    DSub,
    FDiv,
    GetStatic(PoolIndex),
    F2l,
    F2d,
    I2l,
    I2d,
    I2s,
    I2c,
    I2b,
    I2f,
    IALoad,
    IAStore,
    IMul,
    IDiv,
    IAnd,
    INeg,
    InstanceOf(PoolIndex),
    L2i,
    L2d,
    L2f,
    LALoad,
    LAStore,
    LAdd,
    LAnd,
    LOr,
    LXOr,
    LSub,
    LMul,
    LDiv,
    ISub,
    IRem,
    LNeg,
    IShL,
    IShR,
    IUShR,
    IOr,
    IXOr,
    LCmp,
    IReturn,
    LReturn,
    LRem,
    LShL,
    LShR,
    LUShR,
    LookupSwitch(LookupSwitchData),
    Nop,
    MonitorEnter,
    MonitorExit,
    MultiANewArray {
        index: u16,
        dimensions: u8,
    },
    New(PoolIndex),
    AStore(Index),
    LStore(Index),
    IStore(Index),
    DStore(Index),
    FStore(Index),
    FLoad(Index),
    ILoad(Index),
    ALoad(Index),
    DLoad(Index),
    LLoad(Index),
    IInc {
        index: u16,
        value: i16,
    },
    InvokeDynamic(PoolIndex),
    Ret(Index),
    IfACmpEq(BranchIndex),
    IfACmpNe(BranchIndex),
    IfICmpEq(BranchIndex),
    IfICmpNe(BranchIndex),
    IfICmpLt(BranchIndex),
    IfICmpGe(BranchIndex),
    IfICmpGt(BranchIndex),
    IfICmpLe(BranchIndex),
    IfNull(BranchIndex),
    IfNonNull(BranchIndex),
    IfEq(BranchIndex),
    IfNe(BranchIndex),
    IfLt(BranchIndex),
    IfGe(BranchIndex),
    IfGt(BranchIndex),
    IfLe(BranchIndex),
    Goto(BranchIndex),
    JSr(BranchIndex),
}
