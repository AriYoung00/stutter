use std::fmt::{Display, Debug};

pub trait Emit {
    fn emit(&self) -> String;
}


#[derive(Debug, PartialEq, Eq)]
pub enum Val {
    Reg(Reg),

    Imm(i64),

    /// stack index, will be multiplied by 8 to find actual position since we index the stack in
    /// 64-bit words
    StackIndex(i64),
}

impl Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Reg(r) => Display::fmt(&r, f),
            Val::Imm(i) => write!(f, "QWORD {i}"),
            Val::StackIndex(i) => {
                let offset = 8 * i;
                write!(f, "QWORD [rsp - {offset}]")
            }
        }
    }
}


#[derive(Debug, PartialEq, Eq)]
#[allow(dead_code)]
pub enum Reg {
    RAX,
    RBX,
    RCX,
    RDI,
    RSP,
    
    AX, BX, CX,

    AH, BH, CH,
    AL, BL, CL,
}

impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Reg::*;
        match self {
            RAX => write!(f, "RAX"),
            RBX => write!(f, "RBX"),
            RCX => write!(f, "RCX"),
            RDI => write!(f, "RDI"),
            RSP => write!(f, "RSP"),

            AX =>  write!(f, "AX"),
            BX =>  write!(f, "BX"),
            CX =>  write!(f, "CX"),

            AH =>  write!(f, "AH"),
            BH =>  write!(f, "BH"),
            CH =>  write!(f, "CH"),

            AL =>  write!(f, "AL"),
            BL =>  write!(f, "BL"),
            CL =>  write!(f, "CL"),
        }
    }
}


#[derive(Debug, PartialEq, Eq)]
#[allow(dead_code)]
/// This enum represents assembly instructions
pub enum Instr {
    Mov(Val, Val),
    Add(Val, Val),
    Sub(Val, Val),
    IMul(Val, Val),
    Mul(Val, Val),
    And(Val, Val),
    Xor(Val, Val),
    Sar(Val),
    Cmp(Val, Val),
    Test(Val, Val),
    // bit test
//    Bt(Val, Val),

    Jne(String),
    Je(String),
    Jz(String),
    Jnz(String),
    Jc(String),
    Jnc(String),
    Jmp(String),
    Jo(String),

    Call(String),

    /// conditional move ==
    Cmove(Reg, Val),
    /// conditional move !=
    Cmovne(Reg, Val),
    /// conditional move >=
    Cmovge(Reg, Val),
    /// conditional move <=
    Cmovle(Reg, Val),
    /// conditional move <
    Cmovl(Reg, Val),
    /// conditional move >
    Cmovg(Reg, Val),
    /// conditional move if CF == 1
    Cmovc(Reg, Val),
    /// conditional move if CF == 0
    Cmovnc(Reg, Val),
    /// conditional move if zero flat is set
    Cmovz(Reg, Val),
    /// conditional move if zero flag is not set
    Cmovnz(Reg, Val),

    // conditional set (1 or 0 based on condition)
    Setz(Reg),
    Setnz(Reg),

    Push(Val),
    Pop(Val),
}

use std::fmt::Formatter;
impl Display for Instr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Reg::*;
        // quick hack to handle the special case for conditional moves -- neither operand can be
        // an immediate, so we need to include an additional `mov` into a scratch register
        // for now, this is rbx
        let cmov_hack = |f: &mut Formatter<'_>, inst: &str, dest: &Reg, src: &Val| match src {
            Val::Imm(i) => write!(f, "mov rbx, {i}\n\t{inst} {dest}, rbx"),
            _ => write!(f, "{inst} {dest}, {src}")
        };

        let mul_hack = |f: &mut Formatter<'_>, inst: &str, dest: &Val, src: &Val| {
            match (dest, src) {
                (Val::Reg(RAX), _) => write!(f, "{inst} {src}"),
                // (_, Val::Reg(Reg::RAX)) => format!("imul {}", dest.emit()),
                _ => panic!("this is that stupid bug you didn't fix coming back up -- IMul dest is not RAX"),
            }
        };

        match self {
            Instr::Mov(dest, src)  => write!(f, "mov {dest}, {src}"),
            Instr::Add(dest, src)  => write!(f, "add {dest}, {src}"),
            Instr::Sub(dest, src)  => write!(f, "sub {dest}, {src}"),
            Instr::Mul(dest, src)  => mul_hack(f, "mul",  dest, src),
            Instr::IMul(dest, src) => mul_hack(f, "imul", dest, src),
            Instr::And(dest, src)  => write!(f, "and {dest}, {src}"),
            Instr::Xor(dest, src)  => write!(f, "xor {dest}, {src}"),
            Instr::Sar(dest)       => write!(f, "sar {dest}, 1"),
            Instr::Cmp(lhs, rhs)   => write!(f, "cmp {lhs},  {rhs}"),
            Instr::Test(lhs, rhs)  => write!(f, "test {lhs}, {rhs}"),

            Instr::Jne(l) => write!(f, "jne {l}"),
            Instr::Je(l)  => write!(f, "je  {l}"),
            Instr::Jz(l)  => write!(f, "jz  {l}"),
            Instr::Jnz(l) => write!(f, "jnz {l}"),
            Instr::Jc(l)  => write!(f, "jc  {l}"),
            Instr::Jo(l)  => write!(f, "jo  {l}"),
            Instr::Jnc(l) => write!(f, "jnc {l}"),
            Instr::Jmp(l) => write!(f, "jmp {l}"),
            Instr::Call(l) => write!(f, "call {l}"),

            Instr::Cmove(dest, src)  => cmov_hack(f, "cmove",  dest, src),
            Instr::Cmovne(dest, src) => cmov_hack(f, "cmovne", dest, src),
            Instr::Cmovge(dest, src) => cmov_hack(f, "cmovge", dest, src),
            Instr::Cmovle(dest, src) => cmov_hack(f, "cmovle", dest, src),
            Instr::Cmovl(dest, src)  => cmov_hack(f, "cmovl",  dest, src),
            Instr::Cmovg(dest, src)  => cmov_hack(f, "cmovg",  dest, src),
            Instr::Cmovc(dest, src)  => cmov_hack(f, "cmovgc", dest, src),
            Instr::Cmovnc(dest, src) => cmov_hack(f, "cmovnc", dest, src),
            Instr::Cmovz(dest, src)  => cmov_hack(f, "cmovz",  dest, src),
            Instr::Cmovnz(dest, src) => cmov_hack(f, "cmovnz", dest, src),

            Instr::Push(src) => write!(f, "push {src}"),
            Instr::Pop(dest) => write!(f, "pop  {dest}"),

            Instr::Setz(reg) => match reg {
                AL | BL | CL | AH | BH | CH => write!(f, "setz {reg}"),
                _ => panic!("Can only use Setz with 8-bit registers")
            }
            Instr::Setnz(reg) => match reg {
                AL | BL | CL | AH | BH | CH => write!(f, "setz {reg}"),
                _ => panic!("Can only use Setz with 8-bit registers")
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum AssemblyLine {
    Instruction(Instr),
    Label(String),
}

impl Emit for AssemblyLine {
    fn emit(&self) -> String {
        use AssemblyLine::*;
        match self {
            Instruction(i) => format!("\t{i}"),
            Label(l) => format!("\n{l}:"),
        }
    }
}

impl From<Instr> for AssemblyLine {
    fn from(value: Instr) -> Self {
        AssemblyLine::Instruction(value)
    }
}

