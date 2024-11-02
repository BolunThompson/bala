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
                println!("{} {:#?}", dest, vars);
                let del = vars.get(dest).is_some_and(|v| *v == 0);
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
    // TODO: What should I name this?
    usage_count,
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
        // TODO: Problem is that prev is always empty
        for instr in code {
            if let Some(dest) = instr.dest()
                && let Some(val) = instr.exec(instr.args(), &prev)
            {
                prev.insert(dest.clone(), val);
            }
        }
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
        var: &K,
    ) -> Result<V, Box<dyn Error>> {
        let prog_file = File::open(fname)?;
        let program: Program =
            parse_abstract_program_from_read(prog_file, true, true, None).try_into()?;
        let mut cfg = Cfg::new(
            program
                .functions
                .iter()
                .find(|v| v.name == "main")
                .expect("no main function"),
        );
        let (_in_, mut out) = dfa(&mut cfg);
        cfg.exactly_last()
            .map(|ref ni| {
                println!("{:#?} {:#?}", out, var);
                out.remove(ni)
                    .unwrap()
                    .remove(var)
                    .expect("var not present")
            })
            .ok_or("not only one terminating node".into())
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
    #[test]
    fn test_dce() -> Result<(), Box<dyn Error>> {
        // TODO: Add more
        assert_eq!(test_out("samples/tdce/combo.bril", dce_prog)?, 4);
        Ok(())
    }

    // #[test]
    // fn test_usage_count() -> Result<(), Box<dyn Error>> {
    //     let test_cases = [
    //         ("clobber.bril", "", 9),
    //         ("clobber-arg.bril", "", 4),
    //         ("commute.bril", "", 7),
    //         ("idchain.bril", "", 6),
    //         ("idchain-nonlocal.bril", "", 8),
    //     ];
    //     for (tc, var, value) in test_cases {
    //         println!("#### \x1b[1m{}\x1b[0m", tc);
    //         assert_eq!(
    //             test_dfa_main(
    //                 &format!("samples/lvn/{}", tc),
    //                 usage_count,
    //                 &var.to_string()
    //             )?,
    //             value
    //         );
    //     }
    //     Ok(())
    // }
    #[test]
    fn test_const_expr() -> Result<(), Box<dyn Error>> {
        let test_cases = [
            ("clobber.bril", "prod2", 36),
            ("clobber-arg.bril", "b", 3),
            ("commute.bril", "prod", 36),
            ("idchain.bril", "copy3", 4),
            ("idchain-nonlocal.bril", "copy3", 4),
        ];
        for (tc, var, value) in test_cases {
            println!("#### \x1b[1m{}\x1b[0m", tc);
            assert_eq!(
                test_dfa_main(&format!("samples/lvn/{}", tc), const_prop, &var.to_string())?,
                bril_rs::Literal::Int(value)
            );
        }
        Ok(())
    }

    #[test]
    fn test_cse() -> Result<(), Box<dyn Error>> {
        let test_cases = [
            ("clobber.bril", "prod2", 36),
            ("clobber-arg.bril", "b", 3),
            ("commute.bril", "prod", 36),
            ("idchain.bril", "copy3", 4),
            ("idchain-nonlocal.bril", "copy3", 4),
        ];
        for (tc, var, value) in test_cases {
            println!("#### \x1b[1m{}\x1b[0m", tc);
            assert_eq!(
                test_dfa_main(&format!("samples/lvn/{}", tc), const_prop, &var.to_string())?,
                bril_rs::Literal::Int(value)
            );
        }
        Ok(())
    }
    // TODO: Test CSE.
}
