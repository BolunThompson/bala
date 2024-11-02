use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
    hash::Hash,
    mem,
};

use serde::{Deserialize, Serialize};

/// Equivalent to a file of bril code
#[cfg_attr(not(feature = "float"), derive(Eq))]
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Program {
    /// A list of functions declared in the program
    pub functions: Vec<Function>,
    #[cfg(feature = "import")]
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    /// A list of imports for this program
    pub imports: Vec<Import>,
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        #[cfg(feature = "import")]
        for i in &self.imports {
            writeln!(f, "{i}")?;
        }
        for func in &self.functions {
            writeln!(f, "{func}")?;
        }
        Ok(())
    }
}

/// <https://capra.cs.cornell.edu/bril/lang/import.html#syntax>
#[cfg(feature = "import")]
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Import {
    /// A list of functions to be imported
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub functions: Vec<ImportedFunction>,
    /// The relative path of the file from some lib directory specified by the user
    pub path: std::path::PathBuf,
}

#[cfg(feature = "import")]
impl Display for Import {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "from {}", self.path.display())?;
        if !self.functions.is_empty() {
            write!(f, " import ")?;
            for (i, name) in self.functions.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{name}")?;
            }
        }
        write!(f, ";")?;
        Ok(())
    }
}

/// <https://capra.cs.cornell.edu/bril/lang/import.html#syntax>
#[cfg(feature = "import")]
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ImportedFunction {
    #[serde(skip_serializing_if = "Option::is_none")]
    /// A function can be optionally aliased with a different name for use in the rest of the program
    pub alias: Option<String>,
    /// The name of the function being imported
    pub name: String,
}

#[cfg(feature = "import")]
impl Display for ImportedFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(a) = self.alias.as_ref() {
            write!(f, " as {a}")?;
        }
        Ok(())
    }
}

/// <https://capra.cs.cornell.edu/bril/lang/syntax.html#function>
#[cfg_attr(not(feature = "float"), derive(Eq))]
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Function {
    /// Any arguments the function accepts
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub args: Vec<Argument>,
    /// The instructions of this function
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub instrs: Vec<Code>,
    /// The name of the function
    pub name: String,
    /// The position of this function in the original source code
    #[cfg(feature = "position")]
    #[serde(flatten, skip_serializing_if = "Option::is_none")]
    pub pos: Option<Position>,
    /// The possible return type of this function
    #[serde(rename = "type")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub return_type: Option<Type>,
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "@{}", self.name)?;
        if !self.args.is_empty() {
            write!(f, "(")?;
            for (i, arg) in self.args.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{arg}")?;
            }
            write!(f, ")")?;
        }
        if let Some(tpe) = self.return_type.as_ref() {
            write!(f, ": {tpe}")?;
        }
        writeln!(f, " {{")?;
        for instr in &self.instrs {
            writeln!(f, "{instr}")?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

/// An argument of a function
/// <https://capra.cs.cornell.edu/bril/lang/syntax.html#function>
/// Example: a : int
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Argument {
    /// a
    pub name: String,
    #[serde(rename = "type")]
    /// int
    pub arg_type: Type,
}

impl Display for Argument {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.arg_type)
    }
}

/// <https://capra.cs.cornell.edu/bril/lang/syntax.html#function>
/// Code is a Label or an Instruction
#[cfg_attr(not(feature = "float"), derive(Eq))]
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(untagged)]
pub enum Code {
    /// <https://capra.cs.cornell.edu/bril/lang/syntax.html#label>
    Label {
        /// The name of the label
        label: String,
        /// Where the label is located in source code
        #[cfg(feature = "position")]
        #[serde(flatten, skip_serializing_if = "Option::is_none")]
        pos: Option<Position>,
    },
    /// <https://capra.cs.cornell.edu/bril/lang/syntax.html#instruction>
    Instruction(Instruction),
}

impl Display for Code {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Label {
                label,
                #[cfg(feature = "position")]
                    pos: _,
            } => write!(f, ".{label}:"),
            Self::Instruction(instr) => write!(f, "  {instr}"),
        }
    }
}

impl Code {
    /// checks if code is terminator
    pub fn is_term(&self) -> bool {
        match &self {
            Self::Label { .. } => true,
            Self::Instruction(instr) if instr.is_term() => true,
            _ => false,
        }
    }
}

/// <https://capra.cs.cornell.edu/bril/lang/syntax.html#instruction>
#[cfg_attr(not(feature = "float"), derive(Eq))]
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(untagged)]
pub enum Instruction {
    /// <https://capra.cs.cornell.edu/bril/lang/syntax.html#constant>
    Constant {
        /// destination variable
        dest: String,
        /// "const"
        op: ConstOps,
        #[cfg(feature = "position")]
        /// The source position of the instruction if provided
        #[serde(flatten, skip_serializing_if = "Option::is_none")]
        pos: Option<Position>,
        /// Type of variable
        #[serde(rename = "type")]
        const_type: Type,
        /// The literal being stored in the variable
        value: Literal,
    },
    /// <https://capra.cs.cornell.edu/bril/lang/syntax.html#value-operation>
    Value {
        /// List of variables as arguments
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        args: Vec<String>,
        /// destination variable
        dest: String,
        /// List of strings as function names
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        funcs: Vec<String>,
        /// List of strings as labels
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        labels: Vec<String>,
        /// Operation being executed
        op: ValueOps,
        /// The source position of the instruction if provided
        #[cfg(feature = "position")]
        #[serde(flatten, skip_serializing_if = "Option::is_none")]
        pos: Option<Position>,
        /// Type of variable
        #[serde(rename = "type")]
        op_type: Type,
    },
    /// <https://capra.cs.cornell.edu/bril/lang/syntax.html#effect-operation>
    Effect {
        /// List of variables as arguments
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        args: Vec<String>,
        /// List of strings as function names
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        funcs: Vec<String>,
        /// List of strings as labels
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        labels: Vec<String>,
        /// Operation being executed
        op: EffectOps,
        /// The source position of the instruction if provided
        #[cfg(feature = "position")]
        #[serde(flatten, skip_serializing_if = "Option::is_none")]
        pos: Option<Position>,
    },
}

impl Default for Instruction {
    fn default() -> Self {
        Self::Constant {
            dest: "".to_string(),
            op: ConstOps::Const,
            pos: None,
            const_type: Type::Int,
            value: Literal::Int(0),
        }
    }
}

#[cfg(feature = "position")]
impl Instruction {
    /// A helper function to extract the position value if it exists from an instruction
    #[must_use]
    pub fn get_pos(&self) -> Option<Position> {
        match self {
            Self::Constant { pos, .. } | Self::Value { pos, .. } | Self::Effect { pos, .. } => {
                pos.clone()
            }
        }
    }
    /// checks if the expressions of two instructions are equal (ignores dest)
    pub fn expr_eq(&self, other: &Self) -> bool {
        match (self, other) {
            // assumes well formed floats (ie: no NaNs)
            (&Self::Constant { value: v1, .. }, &Self::Constant { value: v2, .. }) => v1 == v2,
            (
                &Self::Value {
                    args: ref args1,
                    labels: ref labels1,
                    funcs: ref funcs1,
                    ..
                },
                &Self::Value {
                    args: ref args2,
                    labels: ref labels2,
                    funcs: ref funcs2,
                    ..
                },
            ) => args1 == args2 && labels1 == labels2 && funcs1 == funcs2,
            _ => false,
        }
    }

    // converts self into id instr
    pub fn id_self(&mut self, from: String) {
        *self = Instruction::Value {
            args: vec![from], // Assuming dest is a reference, dereference it if necessary
            dest: mem::take(self.dest_mut().expect("can't instr with no dest!")),
            funcs: Vec::with_capacity(0),
            labels: Vec::with_capacity(0),
            op: ValueOps::Id,
            pos: None,
            op_type: self.op_type().unwrap(),
        }
    }

    /// exec instruction if possible bassed on arg values
    pub fn exec(
        &self,
        args: Option<&Vec<String>>,
        cvals: &HashMap<String, Literal>,
    ) -> Option<Literal> {
        let arg0 = args
            .and_then(|args| args.get(0))
            .and_then(|v| cvals.get(v))
            .map(|v| *v);
        let arg1 = args
            .and_then(|args| args.get(1))
            .and_then(|v| cvals.get(v))
            .map(|v| *v);
        // if self.dest().map_or("", |v| v) == "prod2" {
        //     println!("PROD2: {:#?}, {:#?}, {:#?}, {:#?}", args, cvals, arg0, arg1);
        // }
        match self {
            Self::Value { op, .. } => op.exec(arg0, arg1),
            Self::Constant { value, .. } => Some(*value),
            _ => None,
        }
    }

    /// turns function into canonical form
    pub fn normalize(&mut self) {
        // hack to satisfy the borrow checker
        let associative = self.associative();
        match self {
            Self::Value { args, .. } if associative => args.sort(),
            _ => (),
        }
    }
    /// get args
    pub fn args(&self) -> Option<&Vec<String>> {
        match self {
            Self::Value { args, .. } => Some(args),
            Self::Effect { args, .. } => Some(args),
            _ => None,
        }
    }

    fn associative(&self) -> bool {
        matches!(
            self,
            Self::Value {
                op: ValueOps::Add | ValueOps::Mul | ValueOps::Eq,
                ..
            }
        )
    }

    /// get dest mut of method if it exists
    pub fn dest_mut(&mut self) -> Option<&mut String> {
        match self {
            Self::Constant { dest, .. } => Some(dest),
            Self::Value { dest, .. } => Some(dest),
            Self::Effect { .. } => None,
        }
    }

    /// get dest of method if it exists
    pub fn dest(&self) -> Option<&String> {
        match self {
            Self::Constant { dest, .. } => Some(dest),
            Self::Value { dest, .. } => Some(dest),
            Self::Effect { .. } => None,
        }
    }

    /// checks if instruction is terminator
    pub fn is_term(&self) -> bool {
        matches!(
            self,
            Instruction::Effect {
                op: EffectOps::Branch | EffectOps::Return | EffectOps::Jump,
                ..
            }
        )
    }
    // pub fn number(&mut self, var_count: &mut HashMap<String, u32>) {
    //     if let Some(dest) = self.dest_mut() {
    //         dest.push_str(&format!(
    //             ".{}",
    //             var_count
    //                 .entry(dest.clone())
    //                 .and_modify(|v| *v += 1)
    //                 .or_insert(0)
    //         ))
    //     }
    //     if let Some(args) = self.args_mut() {
    //         for arg in args {
    //             if let Some(vn) = var_count.get(arg) {
    //                 println!("ARG: {:#?}", arg);
    //                 arg.push_str(&format!(".{}", vn));
    //             }
    //         }
    //     }
    // }

    // pub fn unnumber(&mut self) {
    //     if let Some(dest) = self.dest_mut() {
    //         *dest = unnumber_str(dest);
    //     }

    //     if let Some(args) = self.args_mut() {
    //         for arg in args.iter_mut() {
    //             *arg = unnumber_str(arg);
    //         }
    //     }
    // }

    pub fn to_const(&mut self, cvals: &HashMap<String, Literal>) {
        match self {
            Self::Value { dest, .. } => {
                let dest = mem::take(dest);
                let literal = *cvals.get(&dest).expect("dest not in cvals");
                *self = Self::Constant {
                    dest,
                    op: ConstOps::Const,
                    pos: None,
                    const_type: literal.get_type(),
                    value: literal,
                };
            }
            Self::Constant { .. } => (),
            Self::Effect { .. } => panic!("effect instr can't be made const"),
        }
    }

    pub fn op_type(&self) -> Option<Type> {
        match self {
            Self::Value { op_type, .. } => Some(op_type.clone()),
            Self::Constant {
                const_type: op_type,
                ..
            } => Some(op_type.clone()),
            _ => None,
        }
    }
}
// TODO: Move somehwere else where it can be sanely public
/// Remove ".n" numbering from a var
pub fn unnumber_str(str: &str) -> String {
    str.rsplit_once('.')
        .map_or(str, |(base, _)| base)
        .to_string()
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Constant {
                op,
                dest,
                const_type,
                value,
                #[cfg(feature = "position")]
                    pos: _,
            } => {
                write!(f, "{dest}: {const_type} = {op} {value};")
            }
            Self::Value {
                op,
                dest,
                op_type,
                args,
                funcs,
                labels,
                #[cfg(feature = "position")]
                    pos: _,
            } => {
                write!(f, "{dest}: {op_type} = {op}")?;
                for func in funcs {
                    write!(f, " @{func}")?;
                }
                for arg in args {
                    write!(f, " {arg}")?;
                }
                for label in labels {
                    write!(f, " .{label}")?;
                }
                write!(f, ";")
            }
            Self::Effect {
                op,
                args,
                funcs,
                labels,
                #[cfg(feature = "position")]
                    pos: _,
            } => {
                write!(f, "{op}")?;
                for func in funcs {
                    write!(f, " @{func}")?;
                }
                for arg in args {
                    write!(f, " {arg}")?;
                }
                for label in labels {
                    write!(f, " .{label}")?;
                }
                write!(f, ";")
            }
        }
    }
}

/// <https://capra.cs.cornell.edu/bril/lang/syntax.html#constant>
#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ConstOps {
    /// "const"
    #[serde(rename = "const")]
    Const,
}

impl Display for ConstOps {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Const => write!(f, "const"),
        }
    }
}

/// <https://capra.cs.cornell.edu/bril/lang/syntax.html#effect-operation>
#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[serde(rename_all = "lowercase")]
pub enum EffectOps {
    /// <https://capra.cs.cornell.edu/bril/lang/core.html#control>
    #[serde(rename = "jmp")]
    Jump,
    /// <https://capra.cs.cornell.edu/bril/lang/core.html#control>
    #[serde(rename = "br")]
    Branch,
    /// <https://capra.cs.cornell.edu/bril/lang/core.html#control>
    Call,
    /// <https://capra.cs.cornell.edu/bril/lang/core.html#control>
    #[serde(rename = "ret")]
    Return,
    /// <https://capra.cs.cornell.edu/bril/lang/core.html#miscellaneous>
    Print,
    /// <https://capra.cs.cornell.edu/bril/lang/core.html#miscellaneous>
    Nop,
    /// <https://capra.cs.cornell.edu/bril/lang/memory.html#operations>
    #[cfg(feature = "memory")]
    Store,
    /// <https://capra.cs.cornell.edu/bril/lang/memory.html#operations>
    #[cfg(feature = "memory")]
    Free,
    /// <https://capra.cs.cornell.edu/bril/lang/spec.html#operations>
    #[cfg(feature = "speculate")]
    Speculate,
    /// <https://capra.cs.cornell.edu/bril/lang/spec.html#operations>
    #[cfg(feature = "speculate")]
    Commit,
    /// <https://capra.cs.cornell.edu/bril/lang/spec.html#operations>
    #[cfg(feature = "speculate")]
    Guard,
}

impl Display for EffectOps {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Jump => write!(f, "jmp"),
            Self::Branch => write!(f, "br"),
            Self::Call => write!(f, "call"),
            Self::Return => write!(f, "ret"),
            Self::Print => write!(f, "print"),
            Self::Nop => write!(f, "nop"),
            #[cfg(feature = "memory")]
            Self::Store => write!(f, "store"),
            #[cfg(feature = "memory")]
            Self::Free => write!(f, "free"),
            #[cfg(feature = "speculate")]
            Self::Speculate => write!(f, "speculate"),
            #[cfg(feature = "speculate")]
            Self::Commit => write!(f, "commit"),
            #[cfg(feature = "speculate")]
            Self::Guard => write!(f, "guard"),
        }
    }
}

/// <https://capra.cs.cornell.edu/bril/lang/syntax.html#value-operation>
#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[serde(rename_all = "lowercase")]
pub enum ValueOps {
    /// <https://capra.cs.cornell.edu/bril/lang/core.html#arithmetic>
    Add,
    /// <https://capra.cs.cornell.edu/bril/lang/core.html#arithmetic>
    Sub,
    /// <https://capra.cs.cornell.edu/bril/lang/core.html#arithmetic>
    Mul,
    /// <https://capra.cs.cornell.edu/bril/lang/core.html#arithmetic>
    Div,
    /// <https://capra.cs.cornell.edu/bril/lang/core.html#comparison>
    Eq,
    /// <https://capra.cs.cornell.edu/bril/lang/core.html#comparison>
    Lt,
    /// <https://capra.cs.cornell.edu/bril/lang/core.html#comparison>
    Gt,
    /// <https://capra.cs.cornell.edu/bril/lang/core.html#comparison>
    Le,
    /// <https://capra.cs.cornell.edu/bril/lang/core.html#comparison>
    Ge,
    /// <https://capra.cs.cornell.edu/bril/lang/core.html#logic>
    Not,
    /// <https://capra.cs.cornell.edu/bril/lang/core.html#logic>
    And,
    /// <https://capra.cs.cornell.edu/bril/lang/core.html#logic>
    Or,
    /// <https://capra.cs.cornell.edu/bril/lang/core.html#control>
    Call,
    /// <https://capra.cs.cornell.edu/bril/lang/core.html#miscellaneous>
    Id,
    /// <https://capra.cs.cornell.edu/bril/lang/ssa.html#operations>
    #[cfg(feature = "ssa")]
    Phi,
    /// <https://capra.cs.cornell.edu/bril/lang/float.html#operations>
    #[cfg(feature = "float")]
    Fadd,
    /// <https://capra.cs.cornell.edu/bril/lang/float.html#operations>
    #[cfg(feature = "float")]
    Fsub,
    /// <https://capra.cs.cornell.edu/bril/lang/float.html#operations>
    #[cfg(feature = "float")]
    Fmul,
    /// <https://capra.cs.cornell.edu/bril/lang/float.html#operations>
    #[cfg(feature = "float")]
    Fdiv,
    /// <https://capra.cs.cornell.edu/bril/lang/float.html#operations>
    #[cfg(feature = "float")]
    Feq,
    /// <https://capra.cs.cornell.edu/bril/lang/float.html#operations>
    #[cfg(feature = "float")]
    Flt,
    /// <https://capra.cs.cornell.edu/bril/lang/float.html#operations>
    #[cfg(feature = "float")]
    Fgt,
    /// <https://capra.cs.cornell.edu/bril/lang/float.html#operations>
    #[cfg(feature = "float")]
    Fle,
    /// <https://capra.cs.cornell.edu/bril/lang/float.html#operations>
    #[cfg(feature = "float")]
    Fge,
    /// <https://capra.cs.cornell.edu/bril/lang/char.html#operations>
    #[cfg(feature = "char")]
    Ceq,
    /// <https://capra.cs.cornell.edu/bril/lang/char.html#operations>
    #[cfg(feature = "char")]
    Clt,
    /// <https://capra.cs.cornell.edu/bril/lang/char.html#operations>
    #[cfg(feature = "char")]
    Cgt,
    /// <https://capra.cs.cornell.edu/bril/lang/char.html#operations>
    #[cfg(feature = "char")]
    Cle,
    /// <https://capra.cs.cornell.edu/bril/lang/char.html#operations>
    #[cfg(feature = "char")]
    Cge,
    /// <https://capra.cs.cornell.edu/bril/lang/char.html#operations>
    #[cfg(feature = "char")]
    Char2int,
    /// <https://capra.cs.cornell.edu/bril/lang/char.html#operations>
    #[cfg(feature = "char")]
    Int2char,
    /// <https://capra.cs.cornell.edu/bril/lang/memory.html#operations>
    #[cfg(feature = "memory")]
    Alloc,
    /// <https://capra.cs.cornell.edu/bril/lang/memory.html#operations>
    #[cfg(feature = "memory")]
    Load,
    /// <https://capra.cs.cornell.edu/bril/lang/memory.html#operations>
    #[cfg(feature = "memory")]
    PtrAdd,
}

impl ValueOps {
    // chatgpt generated boilerplate
    fn exec(&self, arg1: Option<Literal>, arg2: Option<Literal>) -> Option<Literal> {
        match self {
            ValueOps::Add => match (arg1, arg2) {
                (Some(Literal::Int(a)), Some(Literal::Int(b))) => Some(Literal::Int(a + b)),
                _ => None,
            },
            ValueOps::Sub => match (arg1, arg2) {
                (Some(Literal::Int(a)), Some(Literal::Int(b))) => Some(Literal::Int(a - b)),
                _ => None,
            },
            ValueOps::Mul => match (arg1, arg2) {
                (Some(Literal::Int(a)), Some(Literal::Int(b))) => Some(Literal::Int(a * b)),
                _ => None,
            },
            ValueOps::Div => match (arg1, arg2) {
                (Some(Literal::Int(a)), Some(Literal::Int(b))) => {
                    if b != 0 {
                        Some(Literal::Int(a / b))
                    } else {
                        None // Division by zero
                    }
                }
                _ => None,
            },
            ValueOps::Eq => match (arg1, arg2) {
                (Some(Literal::Int(a)), Some(Literal::Int(b))) => Some(Literal::Bool(a == b)),
                (Some(Literal::Bool(a)), Some(Literal::Bool(b))) => Some(Literal::Bool(a == b)),
                #[cfg(feature = "float")]
                (Some(Literal::Float(a)), Some(Literal::Float(b))) => Some(Literal::Bool(a == b)),
                #[cfg(feature = "char")]
                (Some(Literal::Char(a)), Some(Literal::Char(b))) => Some(Literal::Bool(a == b)),
                _ => None,
            },
            ValueOps::Lt => match (arg1, arg2) {
                (Some(Literal::Int(a)), Some(Literal::Int(b))) => Some(Literal::Bool(a < b)),
                #[cfg(feature = "float")]
                (Some(Literal::Float(a)), Some(Literal::Float(b))) => Some(Literal::Bool(a < b)),
                #[cfg(feature = "char")]
                (Some(Literal::Char(a)), Some(Literal::Char(b))) => Some(Literal::Bool(a < b)),
                _ => None,
            },
            ValueOps::Gt => match (arg1, arg2) {
                (Some(Literal::Int(a)), Some(Literal::Int(b))) => Some(Literal::Bool(a > b)),
                #[cfg(feature = "float")]
                (Some(Literal::Float(a)), Some(Literal::Float(b))) => Some(Literal::Bool(a > b)),
                #[cfg(feature = "char")]
                (Some(Literal::Char(a)), Some(Literal::Char(b))) => Some(Literal::Bool(a > b)),
                _ => None,
            },
            ValueOps::Le => match (arg1, arg2) {
                (Some(Literal::Int(a)), Some(Literal::Int(b))) => Some(Literal::Bool(a <= b)),
                #[cfg(feature = "float")]
                (Some(Literal::Float(a)), Some(Literal::Float(b))) => Some(Literal::Bool(a <= b)),
                #[cfg(feature = "char")]
                (Some(Literal::Char(a)), Some(Literal::Char(b))) => Some(Literal::Bool(a <= b)),
                _ => None,
            },
            ValueOps::Ge => match (arg1, arg2) {
                (Some(Literal::Int(a)), Some(Literal::Int(b))) => Some(Literal::Bool(a >= b)),
                #[cfg(feature = "float")]
                (Some(Literal::Float(a)), Some(Literal::Float(b))) => Some(Literal::Bool(a >= b)),
                #[cfg(feature = "char")]
                (Some(Literal::Char(a)), Some(Literal::Char(b))) => Some(Literal::Bool(a >= b)),
                _ => None,
            },
            ValueOps::Not => match (arg1, arg2) {
                (Some(Literal::Bool(a)), None) => Some(Literal::Bool(!a)),
                _ => None,
            },
            ValueOps::And => match (arg1, arg2) {
                (Some(Literal::Bool(a)), Some(Literal::Bool(b))) => Some(Literal::Bool(a && b)),
                _ => None,
            },
            ValueOps::Or => match (arg1, arg2) {
                (Some(Literal::Bool(a)), Some(Literal::Bool(b))) => Some(Literal::Bool(a || b)),
                _ => None,
            },
            ValueOps::Id => match arg2 {
                None => arg1, // This returns the original argument if arg2 is None
                Some(_) => None,
            },
            #[cfg(feature = "float")]
            ValueOps::Fadd => match (arg1, arg2) {
                (Some(Literal::Float(a)), Some(Literal::Float(b))) => Some(Literal::Float(a + b)),
                _ => None,
            },
            #[cfg(feature = "float")]
            ValueOps::Fsub => match (arg1, arg2) {
                (Some(Literal::Float(a)), Some(Literal::Float(b))) => Some(Literal::Float(a - b)),
                _ => None,
            },
            #[cfg(feature = "float")]
            ValueOps::Fmul => match (arg1, arg2) {
                (Some(Literal::Float(a)), Some(Literal::Float(b))) => Some(Literal::Float(a * b)),
                _ => None,
            },
            #[cfg(feature = "float")]
            ValueOps::Fdiv => match (arg1, arg2) {
                (Some(Literal::Float(a)), Some(Literal::Float(b))) => {
                    if b != 0.0 {
                        Some(Literal::Float(a / b))
                    } else {
                        None // Division by zero
                    }
                }
                _ => None,
            },
            #[cfg(feature = "float")]
            ValueOps::Feq => match (arg1, arg2) {
                (Some(Literal::Float(a)), Some(Literal::Float(b))) => Some(Literal::Bool(a == b)),
                _ => None,
            },
            #[cfg(feature = "float")]
            ValueOps::Flt => match (arg1, arg2) {
                (Some(Literal::Float(a)), Some(Literal::Float(b))) => Some(Literal::Bool(a < b)),
                _ => None,
            },
            #[cfg(feature = "float")]
            ValueOps::Fgt => match (arg1, arg2) {
                (Some(Literal::Float(a)), Some(Literal::Float(b))) => Some(Literal::Bool(a > b)),
                _ => None,
            },
            #[cfg(feature = "float")]
            ValueOps::Fle => match (arg1, arg2) {
                (Some(Literal::Float(a)), Some(Literal::Float(b))) => Some(Literal::Bool(a <= b)),
                _ => None,
            },
            #[cfg(feature = "float")]
            ValueOps::Fge => match (arg1, arg2) {
                (Some(Literal::Float(a)), Some(Literal::Float(b))) => Some(Literal::Bool(a >= b)),
                _ => None,
            },
            #[cfg(feature = "char")]
            ValueOps::Ceq => match (arg1, arg2) {
                (Some(Literal::Char(a)), Some(Literal::Char(b))) => Some(Literal::Bool(a == b)),
                _ => None,
            },
            #[cfg(feature = "char")]
            ValueOps::Clt => match (arg1, arg2) {
                (Some(Literal::Char(a)), Some(Literal::Char(b))) => Some(Literal::Bool(a < b)),
                _ => None,
            },
            #[cfg(feature = "char")]
            ValueOps::Cgt => match (arg1, arg2) {
                (Some(Literal::Char(a)), Some(Literal::Char(b))) => Some(Literal::Bool(a > b)),
                _ => None,
            },
            #[cfg(feature = "char")]
            ValueOps::Cle => match (arg1, arg2) {
                (Some(Literal::Char(a)), Some(Literal::Char(b))) => Some(Literal::Bool(a <= b)),
                _ => None,
            },
            #[cfg(feature = "char")]
            ValueOps::Cge => match (arg1, arg2) {
                (Some(Literal::Char(a)), Some(Literal::Char(b))) => Some(Literal::Bool(a >= b)),
                _ => None,
            },
            #[cfg(feature = "char")]
            ValueOps::Char2int => match (arg1, arg2) {
                (Some(Literal::Char(c)), None) => Some(Literal::Int(c as i64)),
                _ => None,
            },
            #[cfg(feature = "char")]
            ValueOps::Int2char => match (arg1, arg2) {
                (Some(Literal::Int(i)), None) => {
                    std::char::from_u32(i as u32).map(|c| Literal::Char(c))
                }
                _ => None,
            },
            _ => None,
        }
    }
}

impl Display for ValueOps {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "add"),
            Self::Sub => write!(f, "sub"),
            Self::Mul => write!(f, "mul"),
            Self::Div => write!(f, "div"),
            Self::Eq => write!(f, "eq"),
            Self::Lt => write!(f, "lt"),
            Self::Gt => write!(f, "gt"),
            Self::Le => write!(f, "le"),
            Self::Ge => write!(f, "ge"),
            Self::Not => write!(f, "not"),
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),
            Self::Call => write!(f, "call"),
            Self::Id => write!(f, "id"),
            #[cfg(feature = "ssa")]
            Self::Phi => write!(f, "phi"),
            #[cfg(feature = "float")]
            Self::Fadd => write!(f, "fadd"),
            #[cfg(feature = "float")]
            Self::Fsub => write!(f, "fsub"),
            #[cfg(feature = "float")]
            Self::Fmul => write!(f, "fmul"),
            #[cfg(feature = "float")]
            Self::Fdiv => write!(f, "fdiv"),
            #[cfg(feature = "float")]
            Self::Feq => write!(f, "feq"),
            #[cfg(feature = "float")]
            Self::Flt => write!(f, "flt"),
            #[cfg(feature = "float")]
            Self::Fgt => write!(f, "fgt"),
            #[cfg(feature = "float")]
            Self::Fle => write!(f, "fle"),
            #[cfg(feature = "float")]
            Self::Fge => write!(f, "fge"),
            #[cfg(feature = "char")]
            Self::Ceq => write!(f, "ceq"),
            #[cfg(feature = "char")]
            Self::Clt => write!(f, "clt"),
            #[cfg(feature = "char")]
            Self::Cgt => write!(f, "cgt"),
            #[cfg(feature = "char")]
            Self::Cle => write!(f, "cle"),
            #[cfg(feature = "char")]
            Self::Cge => write!(f, "cge"),
            #[cfg(feature = "char")]
            Self::Char2int => write!(f, "char2int"),
            #[cfg(feature = "char")]
            Self::Int2char => write!(f, "int2char"),
            #[cfg(feature = "memory")]
            Self::Alloc => write!(f, "alloc"),
            #[cfg(feature = "memory")]
            Self::Load => write!(f, "load"),
            #[cfg(feature = "memory")]
            Self::PtrAdd => write!(f, "ptradd"),
        }
    }
}

/// <https://capra.cs.cornell.edu/bril/lang/syntax.html#type>
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
#[serde(rename_all = "lowercase")]
pub enum Type {
    /// <https://capra.cs.cornell.edu/bril/lang/core.html#types>
    #[default]
    Int,
    /// <https://capra.cs.cornell.edu/bril/lang/core.html#types>
    Bool,
    /// <https://capra.cs.cornell.edu/bril/lang/float.html#types>
    #[cfg(feature = "float")]
    Float,
    /// <https://capra.cs.cornell.edu/bril/lang/char.html#types>
    #[cfg(feature = "char")]
    Char,
    /// <https://capra.cs.cornell.edu/bril/lang/memory.html#types>
    #[cfg(feature = "memory")]
    #[serde(rename = "ptr")]
    Pointer(Box<Self>),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::Bool => write!(f, "bool"),
            #[cfg(feature = "float")]
            Self::Float => write!(f, "float"),
            #[cfg(feature = "char")]
            Self::Char => write!(f, "char"),
            #[cfg(feature = "memory")]
            Self::Pointer(tpe) => write!(f, "ptr<{tpe}>"),
        }
    }
}
/// A JSON number/value
#[cfg_attr(not(feature = "float"), derive(Eq, Hash))]
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Copy)]
#[serde(untagged)]
pub enum Literal {
    /// Integers
    Int(i64),
    /// Booleans
    Bool(bool),
    /// Floating Points
    #[cfg(feature = "float")]
    Float(f64),
    /// UTF-16 Characters
    #[cfg(feature = "char")]
    Char(char),
}

// TODO: Implement math ops for Literals for constant folding

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{i}"),
            Self::Bool(b) => write!(f, "{b}"),
            #[cfg(feature = "float")]
            Self::Float(x) => write!(f, "{x}"),
            #[cfg(feature = "char")]
            Self::Char(c) => write!(f, "\'{}\'", escape_char(*c)),
        }
    }
}

#[cfg(feature = "char")]
fn escape_char(c: char) -> String {
    match c {
        '\u{0000}' => "\\0".to_string(),
        '\u{0007}' => "\\a".to_string(),
        '\u{0008}' => "\\b".to_string(),
        '\u{0009}' => "\\t".to_string(),
        '\u{000A}' => "\\n".to_string(),
        '\u{000B}' => "\\v".to_string(),
        '\u{000C}' => "\\f".to_string(),
        '\u{000D}' => "\\r".to_string(),
        c => c.to_string(),
    }
}

impl Literal {
    /// A helper function to get the type of literal values
    #[must_use]
    pub const fn get_type(&self) -> Type {
        match self {
            Self::Int(_) => Type::Int,
            Self::Bool(_) => Type::Bool,
            #[cfg(feature = "float")]
            Self::Float(_) => Type::Float,
            #[cfg(feature = "char")]
            Self::Char(_) => Type::Char,
        }
    }
}

/// <https://capra.cs.cornell.edu/bril/lang/syntax.html#source-positions>
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Position {
    /// Starting position
    pub pos: ColRow,
    /// Optional ending position
    #[serde(skip_serializing_if = "Option::is_none")]
    pub pos_end: Option<ColRow>,
    /// Optional absolute path to source file
    #[serde(skip_serializing_if = "Option::is_none")]
    pub src: Option<String>,
}

/// <https://capra.cs.cornell.edu/bril/lang/syntax.html#source-positions>
#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
pub struct ColRow {
    /// Column
    pub col: u64,
    /// Row
    pub row: u64,
}
