use crate::cfg::Cfg;
use bril_rs::ValueOps;
use bril_rs::{Instruction, Literal};
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::collections::hash_map::Entry;
use std::{hash, mem};

fn dce(instrs: &mut Vec<Instruction>) {
    // it's somewhat boilerplaty to keep ignoring labels,
    // but it's easier than special casing 1 element vecs with only
    let mut vars = instrs
        .iter()
        .filter_map(|instr| match instr {
            Instruction::Effect { args, .. } => Some(("", &args[..])),
            Instruction::Constant { dest, .. } => Some((dest, &[])),
            Instruction::Value { dest, args, .. } => Some((dest, &args)),
        })
        .fold(HashMap::<String, i32>::new(), |mut state, (dest, args)| {
            for k in args {
                if let Some(v) = state.get_mut(k) {
                    *v += 1;
                };
            }
            if !dest.is_empty() && !state.contains_key(dest) {
                state.insert(dest.to_string(), 0);
            };
            state
        });
    // I suspect there are more efficent ways to implement this instead of retaining a vec
    while vars.values().any(|&v| v == 0) {
        instrs.retain(|instr| match instr {
            Instruction::Constant { dest, .. } => {
                // if this was longer I'd factor this out into a new function
                // to be shared between Constant and Value
                let del = vars[dest] == 0;
                if del {
                    vars.remove(dest);
                };
                !del
            }
            Instruction::Value { dest, args, .. } => {
                let del = vars[dest] == 0;
                if del {
                    for k in args {
                        *vars.get_mut(k).unwrap() -= 1;
                    }
                    vars.remove(dest);
                };
                !del
            }
            _ => true,
        })
    }
}

// // Reimpl of bril-rs Literal -- hack to allow the binary comparison of floats
// #[derive(Clone, Copy, Hash, PartialEq, Eq)]
// enum BinLiteral {
//     Int(i64),
//     Bool(bool),
//     Float(u64), // Bits representation of floats
//     Char(char),
// }

// impl From<Literal> for BinLiteral {
//     fn from(value: Literal) -> Self {
//         match value {
//             Literal::Int(v) => BinLiteral::Int(v),
//             Literal::Bool(v) => BinLiteral::Bool(v),
//             Literal::Float(v) => BinLiteral::Float(v.to_bits()),
//             Literal::Char(v) => BinLiteral::Char(v),
//         }
//     }
// }

// #[derive(Clone, Debug)]
// enum ValueTuple {
//     Constant(String, Literal),
//     Value(String, Vec<String>, Vec<String>, Vec<String>),
// }
// impl ValueTuple {
//     fn dest(&self) -> String {
//         // todo: if I want better perf, ref count the var names instead of cloning
//         // (easy in principle but it just leads to annoying bril-rs threading)
//         (match self {
//             Self::Constant(dest, ..) => dest,
//             Self::Value(dest, ..) => dest,
//         })
//         .to_string()
//     }
//     pub fn id_self(&self, to: String, op_type: Type) -> Instruction {
//         Instruction::Value {
//             args: vec![self.dest()], // Assuming dest is a reference, dereference it if necessary
//             dest: to,
//             funcs: Vec::with_capacity(0),
//             labels: Vec::with_capacity(0),
//             op: ValueOps::Id,
//             pos: None,
//             op_type,
//         }
//     }
// }

// impl PartialEq for ValueTuple {
//     fn eq(&self, other: &Self) -> bool {
//         match (self, other) {
//             // assumes well formed floats (ie: no NaNs)
//             (&ValueTuple::Constant(_, v1), &ValueTuple::Constant(_, v2)) => v1 == v2,
//             (
//                 &ValueTuple::Value(_, ref args1, ref labels1, ref funcs1),
//                 &ValueTuple::Value(_, ref args2, ref labels2, ref funcs2),
//             ) => args1 == args2 && labels1 == labels2 && funcs1 == funcs2,
//             _ => false,
//         }
//     }
// }

// Note: Doesn't work -- code for this was bugfixed moved into various DFA passes.
// pub fn lvn(mut instrs: Vec<Instruction>) -> Vec<Instruction> {
//     // All these separate iterations are inefficient -- ideally, I should combine the passes.
//     // let's hope the compiler's smart enough to do so as possible lol
//     // NOTE: All these clones are quite inefficient! If I need to improve perf,
//     // then I should modify the bril-rs code to RC all the strings.
//     // println!("INSTR_LVN {:#?}", instrs);
//     for instr in &mut instrs {
//         // normalizes algebraic identities
//         instr.normalize();
//     }
//     // TODO: Make more readable
//     let mut var2num: HashMap<String, (usize, Option<Literal>)> = instrs
//         .iter()
//         .enumerate()
//         .filter_map(|(i, v)| {
//             v.dest().map(|dest| {
//                 (
//                     dest.clone(),
//                     (
//                         instrs[..i]
//                             .iter()
//                             .rev()
//                             .position(|vt| vt.expr_eq(v))
//                             .unwrap_or(i),
//                         None,
//                     ),
//                 )
//             })
//         })
//         .collect();
//     // could do on same iter as above (if it isn't optimized into that)

//     // common subexpression elimination and dce
//     // indexes due to ned to separate out immutable and mutable
//     // refernces for the borrow checker
//     // TODO: If I wanted to fix, I need to number the vars to account for shadowing use
//     for i in 0..instrs.len() {
//         if let Some(dest) = instrs[i].dest() {
//             let canon_i = var2num[dest].0;
//             if dest == "sum1" {
//                 println!("SUM 1 {:#?}, {}, {}", var2num, canon_i, i);
//             }
//             if canon_i != i {
//                 let source = instrs[canon_i].dest().unwrap().clone();
//                 instrs[i].id_self(source);
//             }
//         }
//     }
//     // constant folding
//     for instr in instrs.iter() {
//         if let Some(dest) = instr.dest() {
//             let vt = &instrs[var2num[dest].0];
//             println!("EXEC {:#?}", vt);
//             if let Instruction::Value { args, .. } = vt
//                 && let Some(arg1) = args.get(0).map(|v| var2num[v].1)
//                 && let Some(arg2) = args.get(1).map(|v| var2num[v].1)
//             {
//                 var2num.get_mut(dest).unwrap().1 = vt.exec(arg1, arg2);
//             }
//         }
//     }
//     // dce
//     // has to be run after constant folding to maximize coverage
//     // subtle: eliminate shadowed vars which are either non canon or canon but unused
//     let mut in_scope_vars: HashSet<String> = HashSet::new();
//     let mut used_vars: HashSet<String> = HashSet::new();
//     instrs
//         .into_iter()
//         .enumerate()
//         .filter(|(i, instr)| {
//             instr
//                 .dest()
//                 .map(|dest| {
//                     let keep = !(in_scope_vars.contains(dest)
//                         && (var2num[dest].0 != *i || !used_vars.contains(dest)));

//                     in_scope_vars.insert(dest.clone());
//                     if let Some(args) = instr.args() {
//                         used_vars.extend(args.clone());
//                     }
//                     keep
//                 })
//                 .unwrap_or(false)
//         })
//         .map(|(_, v)| v)
//         .collect()
// }

pub enum Direction {
    Forward,
    Backward,
}
type DfData<K, V> = HashMap<NodeIndex, HashMap<K, V>>;
// I suspect, if I want to generically impl forwards and backwards, I need to
// have a Vec struct with the appropriate methods a pass the type generically
// Maybe I should be using a macro.
/// T: type of the value being analyzed
macro_rules! worklist {
    ( $K:ty, $V:ty, $opt:ident, $merge:expr, $transfer:expr, $apply:expr, $direction:expr ) => {
        pub fn $opt(cfg: &mut Cfg) -> (DfData<$K, $V>, DfData<$K, $V>) {
            worklist_func($merge, $transfer, $apply, $direction, cfg)
        }
    };
}

worklist!(
    String,
    u32,
    live,
    |inputs| inputs.flatten().collect::<HashMap<String, u32>>(),
    |code, mut prev| {
        // assumed numbered var names
        let dests = code
            .iter()
            .filter_map(|it| it.dest().map(|v| (v.clone(), 0)));
        prev.extend(dests);
        let args: Vec<_> = code.iter().filter_map(|it| it.args()).flatten().collect();
        for arg in args {
            if let Some(var) = prev.get_mut(arg) {
                *var += 1;
            }
        }
        prev
    },
    |_code, _out| {},
    Direction::Forward
);

// constant propogation
worklist!(
    String,
    Literal,
    const_prop,
    |inputs| inputs.flatten().collect::<HashMap<String, Literal>>(),
    |code, mut prev| {
        let dests: Vec<_> = code
            .iter()
            .filter_map(|instr| {
                instr
                    .dest()
                    .map(|dest| (dest, instr.exec(instr.args(), &prev)))
                    .and_then(|(dest, lit)| lit.map(|lit| (dest.clone(), lit)))
            })
            .collect();
        prev.extend(dests);
        prev
    },
    |code, cvals| {
        for instr in code.iter_mut() {
            match instr.dest() {
                Some(dest) if cvals.contains_key(dest) => {
                    instr.to_const(cvals);
                }
                _ => (),
            }
        }
    },
    Direction::Forward
);

// common subexpresion elimination.
worklist!(
    (ValueOps, Vec<String>), // value tuple
    String,                  // canon var
    cse,
    |inputs| inputs.flatten().collect::<HashMap<_, _>>(),
    |code, prev| {
        let mut value_tuples: HashMap<_, _> = code
            .iter()
            .filter_map(|instr| match instr {
                Instruction::Value { op, args, dest, .. } => {
                    let vt = (*op, args.clone());
                    Some((vt, dest.clone()))
                }
                _ => None,
            })
            .collect();
        value_tuples.extend(prev);
        value_tuples
    },
    |code, value_tuples| {
        for instr in code {
            if let Instruction::Value { op, args, .. } = instr
            // can safely take since the instr's args are about to be discarded anyways
                && let Some(from) = value_tuples.get(&(*op, mem::take(args)))
            {
                instr.id_self(from.clone());
            }
        }
    },
    Direction::Forward
);
// copy elision is another obvious one I could implement

// in my LVN impl I was doing dce as a built-in pass but I'm delegating that
// to the dce func now

#[inline]
fn worklist_func<K: Eq + hash::Hash + Clone, V: Clone>(
    merge: impl Fn(&mut dyn Iterator<Item = HashMap<K, V>>) -> HashMap<K, V>,
    transfer: impl Fn(&[Instruction], HashMap<K, V>) -> HashMap<K, V>,
    apply: impl Fn(&mut Vec<Instruction>, &HashMap<K, V>),
    direction: Direction,
    bbs: &mut Cfg,
) -> (DfData<K, V>, DfData<K, V>) {
    let (pred_dir, out_dir) = match direction {
        Direction::Forward => (petgraph::Direction::Incoming, petgraph::Direction::Outgoing),
        Direction::Backward => (petgraph::Direction::Outgoing, petgraph::Direction::Incoming),
    };
    let mut in_: DfData<K, V> = HashMap::new();
    let mut out: DfData<K, V> = HashMap::new();
    let mut wl: VecDeque<_> = bbs.0.node_indices().collect();
    while let Some(bi) = wl.pop_front() {
        let pred = merge(
            &mut bbs
                .0
                .edges_directed(bi, pred_dir)
                .map(|v| out[&v.source()].clone()),
        );
        in_.insert(bi, pred);
        let succ = transfer(&bbs.0[bi].block[..], in_[&bi].clone());
        if let Entry::Occupied(old_succ) = out.entry(bi)
            && old_succ.get().len() != succ.len()
        {
            wl.extend(bbs.0.edges_directed(bi, out_dir).map(|v| v.source()))
        }
        // TODO: WB reruns? I think it'd just do some on the first pass and the rest on the next passes
        apply(&mut bbs.0[bi].block, &succ);
        dce(&mut bbs.0[bi].block);
        out.insert(bi, succ);
    }

    (in_, out)
}

// fn fmt_df<T>(cfg: Cfg, df: (DfData<T>, DfData<T>)) -> String {
//     let result = String::new();
//     for block in cfg.0.node_indices() {}
// }

#[cfg(test)]
mod tests {
    use core::{fmt, str};
    use std::{error::Error, fs::File, io};

    use crate::cfg::bb;

    use super::*;
    // use bril_rs::program
    use bril_rs::{Code, Program};
    use bril2json::parse_abstract_program_from_read;
    use brilirs::{basic_block::BBProgram, interp::execute_main};

    fn test_out(fname: &str, opt: fn(&mut Program)) -> Result<u32, Box<dyn Error>> {
        let prog_file = File::open(fname)?;
        let mut program =
            parse_abstract_program_from_read(prog_file, true, true, None).try_into()?;
        opt(&mut program);
        let mut stdout: Vec<u8> = Vec::new();
        execute_main(
            &(BBProgram::new(program)?),
            &mut stdout,
            &[],
            true,
            io::sink(),
        )
        .expect("execution error");
        println!("{}", str::from_utf8(&stdout).unwrap());
        let char_num = stdout
            .get(stdout.len().saturating_sub(2))
            .map(|v| *v as char);
        let digit = char_num
            .ok_or("Failed to get character")?
            .to_digit(10)
            .ok_or("Character is not a digit")?;
        Ok(digit)
    }

    fn test_main_len(fname: &str, opt: fn(&mut Program)) -> Result<usize, Box<dyn Error>> {
        let prog_file = File::open(fname)?;
        let mut program =
            parse_abstract_program_from_read(prog_file, true, true, None).try_into()?;
        opt(&mut program);
        println!("{:#?}", program);
        program
            .functions
            .iter()
            .find(|f| f.name == "main")
            .ok_or("no main function".into())
            .map(|f| f.instrs.len())
    }

    fn test_dfa_main<K: hash::Hash + Clone + Eq + fmt::Debug, V: Clone + fmt::Debug>(
        fname: &str,
        dfa: fn(&mut Cfg) -> (DfData<K, V>, DfData<K, V>),
    ) -> Result<(), Box<dyn Error>> {
        let prog_file = File::open(fname)?;
        let program: Program =
            parse_abstract_program_from_read(prog_file, true, true, None).try_into()?;
        let (in_, out) = dfa(&mut Cfg::new(
            program
                .functions
                .iter()
                .find(|v| v.name == "main")
                .expect("no main function"),
        ));
        println!("{:#?} {:#?}", in_.values(), out.values());
        Ok(())
    }

    // I might be able to factor this out but I suspect
    // each version of this func for different opts would
    // be slightly different
    pub fn dce_prog(program: &mut Program) {
        for func in &mut program.functions {
            func.instrs = bb(func)
                .map(|mut b| {
                    dce(&mut b.block);
                    let mut elimed: Vec<Code> =
                        b.block.into_iter().map(|i| Code::Instruction(i)).collect();
                    elimed.push(b.term);
                    elimed
                })
                .flatten()
                .collect()
        }
    }
    // pub fn lvn_prog(program: &mut Program) {
    //     for func in &mut program.functions {
    //         func.instrs = bb(func)
    //             .map(|b| {
    //                 let mut elimed: Vec<Code> = lvn(b.block)
    //                     .into_iter()
    //                     .map(|i| Code::Instruction(i))
    //                     .collect();
    //                 elimed.push(b.term);
    //                 println!("{:#?}", elimed);
    //                 elimed
    //             })
    //             .flatten()
    //             .collect();
    //     }
    // }
    #[test]
    fn test_dce() -> Result<(), Box<dyn Error>> {
        // TODO: Add more
        assert_eq!(test_out("samples/tdce/combo.bril", dce_prog)?, 4);
        Ok(())
    }

    // #[test]
    // fn test_lvn() -> Result<(), Box<dyn Error>> {
    //     let test_cases = [
    //         // lazy so the Some(int) is merely the last number the program outputs
    //         // ex: 36 -> test for 6
    //         // includes implicit ret if none
    //         ("clobber.bril", 9, Some(6)),
    //         ("clobber-arg.bril", 4, None),
    //         ("commute.bril", 7, Some(6)),
    //         ("idchain.bril", 6, Some(4)),
    //         ("idchain-nonlocal.bril", 8, Some(4)),
    //     ];
    //     for (tc, exp_instrs, ret) in test_cases {
    //         println!("#### \x1b[1m{}\x1b[0m", tc);
    //         assert_eq!(
    //             test_main_len(&format!("samples/lvn/{}", tc), lvn_prog)?,
    //             exp_instrs
    //         );
    //         assert_eq!(test_out(&format!("samples/lvn/{}", tc), lvn_prog).ok(), ret);
    //     }
    //     Ok(())
    // }

    // #[test]
    // fn test_live() -> Result<(), Box<dyn Error>> {
    //     let test_cases = [
    //         // lazy so the Some(int) is merely the last number the program outputs
    //         // ex: 36 -> test for 6
    //         // includes implicit ret if none
    //         ("clobber.bril", 9),
    //         ("clobber-arg.bril", 4),
    //         ("commute.bril", 7),
    //         ("idchain.bril", 6),
    //         ("idchain-nonlocal.bril", 8),
    //     ];
    //     for (tc, exp_instrs) in test_cases {
    //         println!("#### \x1b[1m{}\x1b[0m", tc);
    //         // assert_eq!(test_dfa_main(&format!("samples/lvn/{}", tc), live)?, ());
    //     }
    //     Ok(())
    // }
}
