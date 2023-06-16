use std::{
    collections::{HashMap, HashSet},
    vec,
};

use classfile::{
    attributes::{Code, CodeException, CodeOffset, InstructionSeq},
    constant_pool::ConstantPool,
    inst::Instruction,
};
use petgraph::{
    algo::{dominators, tarjan_scc},
    prelude::DiGraphMap,
    visit::IntoEdgesDirected,
    Direction,
};
use thiserror::Error;

use crate::expr::{process_instr, Exprent, ProcessError, Stack};

/// Borrowed slice of an instruction set
#[derive(Debug)]
pub struct BorrowedInstrSeq<'set> {
    /// Slice of the actual set
    pub inner: &'set [(CodeOffset, Instruction)],
}

#[derive(Debug, Error)]
pub enum FlowError {
    #[error("Branch jump target not found")]
    Bounds,
}

/// Obtains the index of a branch position within the
/// provided instruction seq
fn branch_index(seq: &InstructionSeq, branch: usize) -> Option<usize> {
    seq.inner.iter().position(|(pos1, _)| *pos1 == branch)
}

pub fn split_jumps<'seq>(
    seq: &'seq InstructionSeq,
    ex: &[CodeException],
) -> Result<Vec<BorrowedInstrSeq<'seq>>, FlowError> {
    // Collects all the jump instructions (Conditional and Goto)
    let mut jumps = Vec::new();

    // Add exception points
    for ex in ex {
        jumps.push(ex.start_pc as usize);
        jumps.push(ex.end_pc as usize);
        jumps.push(ex.handler_pc as usize);
    }

    for (index, (_, instruction)) in seq.inner.iter().enumerate() {
        use Instruction::*;
        // TODO: Error handling instead of unwraps
        match instruction {
            // If statement branches
            IfNe(branch) | IfEq(branch) | IfLe(branch) | IfGe(branch) | IfGt(branch)
            | IfLt(branch) | IfICmpEq(branch) | IfICmpNe(branch) | IfICmpGt(branch)
            | IfICmpGe(branch) | IfICmpLt(branch) | IfICmpLe(branch) => {
                let true_pos: usize =
                    branch_index(seq, *branch as usize).ok_or(FlowError::Bounds)?;
                let false_pos = index + 1;
                jumps.push(true_pos);
                jumps.push(false_pos);
            }
            // Goto jumps
            Goto(branch) => {
                let jump_pos = branch_index(seq, *branch as usize).ok_or(FlowError::Bounds)?;
                jumps.push(jump_pos);
            }
            // Table switches
            TableSwitch { offsets, .. } => {
                for offset in offsets {
                    let jump_pos = branch_index(seq, *offset as usize).ok_or(FlowError::Bounds)?;

                    jumps.push(jump_pos)
                }
            }
            // Lookup switches
            LookupSwitch { pairs, .. } => {
                for (_, offset) in pairs {
                    let jump_pos = branch_index(seq, *offset as usize).ok_or(FlowError::Bounds)?;

                    jumps.push(jump_pos)
                }
            }
            _ => {}
        }
    }

    // Sort and remove duplicates
    jumps.sort();
    jumps.dedup();

    let mut out: Vec<BorrowedInstrSeq<'_>> = Vec::new();

    if jumps.is_empty() {
        // No jumps means the entire set is used
        out.push(BorrowedInstrSeq { inner: &seq.inner });
        return Ok(out);
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

    Ok(out)
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

    pub fn collect_branches(&mut self, input: &InstructionSeq) {
        let (last_pos, last_instr) = self
            .instructions
            .inner
            .last()
            .expect("Instruction set was empty");
        let next = input
            .inner
            .iter()
            .map(|(pos, _)| *pos)
            .find(|pos| pos > last_pos);

        use Instruction::*;
        match last_instr {
            IfNe(branch) | IfEq(branch) | IfLe(branch) | IfGe(branch) | IfGt(branch)
            | IfLt(branch) | IfICmpEq(branch) | IfICmpNe(branch) | IfICmpGt(branch)
            | IfICmpGe(branch) | IfICmpLt(branch) | IfICmpLe(branch) => {
                let next_pos = next.unwrap();
                self.branches.push(*branch as usize);
                self.branches.push(next_pos);
            }
            Goto(branch) => {
                self.branches.push(*branch as usize);
            }
            Return | AReturn | IReturn | LReturn | DReturn | FReturn => {}
            TableSwitch { offsets, .. } => {
                for offset in offsets {
                    let jump = *offset as usize;
                    self.branches.push(jump)
                }
                let next_pos = next.unwrap();
                self.branches.push(next_pos);
            }
            LookupSwitch { pairs, .. } => {
                for (_, offset) in pairs {
                    let jump = *offset as usize;
                    self.branches.push(jump)
                }
                let next_pos = next.unwrap();
                self.branches.push(next_pos);
            }
            _ => {
                let next_pos = next.unwrap();
                self.branches.push(next_pos);
            }
        }
    }
}

pub fn create_blocks<'seq>(
    input: &'seq InstructionSeq,
    ex: &[CodeException],
) -> Result<HashMap<usize, Block<'seq>>, FlowError> {
    Ok(split_jumps(input, ex)?
        .into_iter()
        .map(|set| {
            let first_pos = set.inner[0].0;

            (
                first_pos,
                Block {
                    start: first_pos,
                    instructions: set,
                    branches: Vec::new(),
                },
            )
        })
        .collect())
}

pub struct ControlFlowGraph<'seq> {
    pub blocks: HashMap<usize, Block<'seq>>,
    pub graph: DiGraphMap<usize, i32>,
    last_id: usize,
}

impl<'seq> ControlFlowGraph<'seq> {
    fn first(&self) -> &Block<'seq> {
        self.blocks.get(&0).expect("Control flow missing first")
    }

    fn last(&self) -> &Block<'seq> {
        self.blocks
            .get(&self.last_id)
            .expect("Control flow missing last")
    }
}

pub fn control_flow(code: &Code) -> Result<ControlFlowGraph<'_>, FlowError> {
    let input = &code.code;
    let mut blocks = create_blocks(input, &code.exception_table)?;
    let mut graph = DiGraphMap::new();

    // Create initial graph points
    blocks.iter().for_each(|(pos, _)| {
        graph.add_node(*pos);
    });

    blocks.iter().for_each(|(pos, block)| {
        // Add edges
        block.branches.iter().for_each(|branch| {
            graph.add_edge(*pos, *branch, 1);
            // branch = successor
        });
    });

    let last = Block {
        start: input.inner.len(),
        instructions: BorrowedInstrSeq { inner: &[] },
        branches: vec![],
    };

    graph.add_node(last.start);

    blocks.iter().for_each(|(pos, _)| {
        // If the block has no sucessors
        if graph
            .edges_directed(*pos, Direction::Outgoing)
            .next()
            .is_none()
        {
            // Make it the block before last
            graph.add_edge(last.start, *pos, 0);
            // graph.add_edge(*pos, last.start, 0);
        }
    });

    let last_id = last.start;

    blocks.insert(last_id, last);

    Ok(ControlFlowGraph {
        blocks,
        graph,
        last_id,
    })
}

pub fn model_control_flow<'seq>(
    input: &'seq InstructionSeq,
    ex: &[CodeException],
) -> Result<Vec<Vec<Block<'seq>>>, FlowError> {
    let mut blocks = create_blocks(input, ex)?;
    let mut graph = DiGraphMap::new();

    // Create initial graph points
    blocks.iter().for_each(|(pos, _)| {
        graph.add_node(*pos);
    });

    blocks.iter().for_each(|(pos, block)| {
        // Add edges
        block.branches.iter().for_each(|branch| {
            graph.add_edge(*pos, *branch, 1);
            // branch = successor
        });
    });

    let first = blocks.get(&0).expect("Missing first block");
    let last = Block {
        start: input.inner.len(),
        instructions: BorrowedInstrSeq { inner: &[] },
        branches: vec![],
    };

    graph.add_node(last.start);

    blocks.iter().for_each(|(pos, _)| {
        // If the block has no sucessors
        if graph
            .edges_directed(*pos, Direction::Outgoing)
            .next()
            .is_none()
        {
            // Make it the block before last
            graph.add_edge(last.start, *pos, 0);
            // graph.add_edge(*pos, last.start, 0);
        }
    });

    blocks.insert(last.start, last);

    let a = dominators::simple_fast(&graph, 0);
    let mut targ = tarjan_scc(&graph);
    targ.sort();
    println!("doms: {:?}", a);

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

    Ok(out1)
}
