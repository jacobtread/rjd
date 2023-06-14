use classfile::attributes::InstructionSeq;
use petgraph::{
    algo::{dominators, tarjan_scc},
    prelude::DiGraphMap,
};

use crate::gen::{create_blocks, Block};

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
