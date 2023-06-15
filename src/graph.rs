use std::collections::HashMap;

use classfile::{
    attributes::{CodeOffset, InstructionSeq},
    constant_pool::ConstantPool,
    inst::Instruction,
};
use petgraph::{
    algo::{dominators, tarjan_scc},
    prelude::DiGraphMap,
};

use crate::expr::{process_instr, Exprent, ProcessError, Stack};

/// Borrowed slice of an instruction set
#[derive(Debug)]
pub struct BorrowedInstrSeq<'set> {
    /// Slice of the actual set
    pub inner: &'set [(CodeOffset, Instruction)],
}

pub fn split_jumps(seq: &InstructionSeq) -> Vec<BorrowedInstrSeq<'_>> {
    // Collects all the jump instructions (Conditional and Goto)
    let mut jumps = Vec::new();
    seq.inner
        .iter()
        .enumerate()
        .for_each(|(index, (_pos, instr))| {
            use Instruction::*;

            // TODO: Error handling instead of unwraps
            match instr {
                IfNe(branch) | IfEq(branch) | IfLe(branch) | IfGe(branch) | IfGt(branch)
                | IfLt(branch) | IfICmpEq(branch) | IfICmpNe(branch) | IfICmpGt(branch)
                | IfICmpGe(branch) | IfICmpLt(branch) | IfICmpLe(branch) => {
                    let true_pos = index_of_position(seq, *branch as usize).unwrap();
                    let false_pos = index + 1;
                    jumps.push(true_pos);
                    jumps.push(false_pos);
                }
                Goto(branch) => {
                    let jump = index_of_position(seq, *branch as usize).unwrap();
                    jumps.push(jump);
                }
                TableSwitch { offsets, .. } => {
                    for offset in offsets {
                        let jump = index_of_position(seq, *offset as usize).unwrap();
                        jumps.push(jump)
                    }
                }
                LookupSwitch { pairs, .. } => {
                    for (_, offset) in pairs {
                        let jump = index_of_position(seq, *offset as usize).unwrap();
                        jumps.push(jump)
                    }
                }
                _ => {}
            }
        });

    // Sort and remove duplicates
    jumps.sort();
    jumps.dedup();

    let mut out: Vec<BorrowedInstrSeq<'_>> = Vec::new();

    if jumps.is_empty() {
        // No jumps means the entire set is used
        out.push(BorrowedInstrSeq { inner: &seq.inner });
        return out;
    }

    // Remove jump to first instruction
    if jumps[0] == 0 {
        jumps.remove(0);
    }

    let jumps_len = jumps.len();

    // Remove end jumping to end
    {
        let last = jumps[jumps_len - 1];
        if last == seq.inner.len() {
            jumps.pop();
        }
    }

    let mut slice: &[(usize, Instruction)] = &seq.inner;

    for i in 0..jumps_len {
        let index = jumps[i] - if i == 0 { 0 } else { jumps[i - 1] };
        let (first, second) = slice.split_at(index);

        out.push(BorrowedInstrSeq { inner: first });

        if i + 1 == jumps_len {
            out.push(BorrowedInstrSeq { inner: second });
            break;
        }

        slice = second;
    }

    out
}

pub fn index_of_position(set: &InstructionSeq, position: usize) -> Option<usize> {
    set.inner
        .iter()
        .enumerate()
        .find_map(|(index, (pos, _))| if *pos == position { Some(index) } else { None })
}

/// Block of instructions
#[derive(Debug)]
pub struct Block<'set> {
    pub start: usize,
    pub instructions: BorrowedInstrSeq<'set>,
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
            if let Err(err) = process_instr(instr, pool, &mut stack, &mut ast) {
                eprintln!("ERR: {:?}", err);
                break;
            }
        }

        Ok(ast)
    }
}

pub fn create_blocks(input: &InstructionSeq) -> HashMap<usize, Block<'_>> {
    split_jumps(input)
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

pub fn model_control_flow(input: &InstructionSeq) -> Vec<Vec<Block<'_>>> {
    let mut blocks = create_blocks(input);
    let mut graph = DiGraphMap::new();

    // Create initial graph points
    blocks.iter().for_each(|(pos, _)| {
        graph.add_node(*pos);
    });

    blocks.iter().for_each(|(pos, block)| {
        // Add edges
        block.branches.iter().for_each(|branch| {
            graph.add_edge(*pos, *branch, 0);
        });
    });

    let a = dominators::simple_fast(&graph, 0);
    let mut targ = tarjan_scc(&graph);

    targ.sort();

    println!("doms: {:?}", a);

    let root = a.root();

    let mut out1 = Vec::with_capacity(targ.len());
    for mut i in targ {
        let mut out2 = Vec::with_capacity(i.len());
        i.sort();
        for i2 in i {
            let value = blocks.remove(&i2).unwrap();
            out2.push(value);
        }
        out1.push(out2);
    }

    out1
}
