use std::collections::HashMap;

use bril_rs::{Code, EffectOps, Function, Instruction};
use itertools::Itertools;
use petgraph::{Direction, Graph, graph::NodeIndex};

pub struct BasicBlock {
    pub term: Code,
    pub block: Vec<Instruction>,
}

type InnerCfg = Graph<BasicBlock, ()>;

pub struct Cfg(pub InnerCfg);

impl Cfg {
    pub fn new(func: &Function) -> Self {
        let mut cfg: InnerCfg = Graph::new();
        let mut linds = HashMap::new();
        let mut prev_ndi = None;
        for block in bb(func) {
            let ndi = cfg.add_node(block);
            if let Some(pndi) = prev_ndi {
                cfg.add_edge(pndi, ndi, ());
                if let Code::Label { ref label, .. } = cfg[pndi].term {
                    linds.insert(label.clone(), ndi);
                }
            }
            prev_ndi = Some(ndi);
        }
        for ind in cfg.node_indices() {
            if let Code::Instruction(Instruction::Effect {
                op: EffectOps::Jump | EffectOps::Branch,
                ref labels,
                ..
            }) = cfg[ind].term
            {
                cfg.add_edge(ind, linds[&labels[0]], ());
            }
        }
        Self(cfg)
    }
    /// returns last node, only if there is only one
    pub fn exactly_last(&self) -> Option<NodeIndex> {
        self.0.externals(Direction::Outgoing).exactly_one().ok()
    }
}

// BBs and their terminators
pub fn bb(func: &Function) -> impl Iterator<Item = BasicBlock> {
    func.instrs
        .chunk_by(|c1, _| !c1.is_term()) // chunk_by never produces an empty chunk, I believe.
        .map(|v| {
            let ret_none = &Code::Instruction(Instruction::Effect {
                op: EffectOps::Return,
                args: Vec::with_capacity(0),
                funcs: Vec::with_capacity(0),
                labels: Vec::with_capacity(0),
                pos: None,
            });
            let (term, block) = if !v.last().unwrap().is_term() {
                (ret_none, v)
            } else {
                v.split_last().unwrap()
            };
            let instrs: Vec<_> = block
                .iter()
                .map(|b| match b {
                    Code::Instruction(instr) => instr.clone(),
                    _ => unreachable!(),
                })
                .collect();

            BasicBlock {
                term: term.clone(),
                block: instrs,
            }
        })
}
