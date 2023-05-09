pub trait Emit {
    fn emit(&self) -> String;
}

impl Emit for i64 {
    fn emit(&self) -> String {
        format!("QWORD {}", self)
    }
}

impl Emit for usize {
    fn emit(&self) -> String {
        format!("QWORD {}", self)
    }
}


#[derive(Debug)]
pub enum Val {
    Reg(Reg),

    Imm(i64),

    /// stack index, will be multiplied by 8 to find actual position since we index the stack in
    /// 64-bit words
    StackIndex(usize),
}

impl Emit for Val {
    fn emit(&self) -> String {
        match self {
            Val::Reg(r) => r.emit(),
            Val::Imm(i) => i.emit(),
            Val::StackIndex(i) => {
                let offset = 8 * i;
                format!("QWORD [rsp - {offset}]")
            }
        }
    }
}


#[derive(Debug)]
pub enum Reg {
    RAX,
    RBX,
    RCX,
    RSP,
}

impl Emit for Reg {
    fn emit(&self) -> String {
        use Reg::*;
        match self {
            RAX => "RAX",
            RBX => "RBX",
            RCX => "RCX",
            RSP => "RSP",
        }.to_owned()
    }
}


#[derive(Debug)]
/// This enum represents assembly instructions
pub enum Instr {
    Mov(Val, Val),
    Add(Val, Val),
    Sub(Val, Val),
    IMul(Val, Val),
    Cmp(Val, Val),
    Test(Val, Val),
    // bit test
    Bt(Val, Val),

    Jne(String),
    Je(String),
    Jz(String),
    Jnz(String),
    Jc(String),
    Jnc(String),
    Jmp(String),
    Jo(String),

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

}

impl Emit for Instr {
    fn emit(&self) -> String {
        // quick hack to handle the special case for conditional moves -- neither operand can be
        // an immediate, so we need to include an additional `mov` into a scratch register
        // for now, this is rdi
        let cmov_hack = |inst: &str, dest: &Reg, src: &Val| match src {
            Val::Imm(i) => format!("mov rdi, {i}\n\t{inst} {}, rdi", dest.emit()),
            _ => format!("{inst} {}, {}", dest.emit(), src.emit())
        };

        match self {
            Instr::Mov(dest, src) => format!("mov {}, {}", dest.emit(), src.emit()),
            Instr::Add(dest, src) => format!("add {}, {}", dest.emit(), src.emit()),
            Instr::Sub(dest, src) => format!("sub {}, {}", dest.emit(), src.emit()),
            Instr::IMul(dest, src) => match (dest, src) {
                (Val::Reg(Reg::RAX), _) => format!("imul {}", src.emit()),
                (_, Val::Reg(Reg::RAX)) => format!("imul {}", dest.emit()),
                _ => panic!("this is that stupid bug you didn't fix coming back up -- neither IMul operand is RAX"),
            }
            Instr::Cmp(lhs, rhs) => format!("cmp {d_cmp}, {s_cmp}", d_cmp=lhs.emit(),
                s_cmp=rhs.emit()),
            Instr::Test(lhs, rhs) => format!("test {d_cmp}, {s_cmp}", d_cmp=lhs.emit(),
                s_cmp=rhs.emit()),
            Instr::Bt(word, bit) => format!("bt {d_cmp}, {s_cmp}", d_cmp=word.emit(),
                s_cmp=bit.emit()),
            Instr::Jne(l) => format!("jne {l}"),
            Instr::Je(l) => format!("je {l}"),
            Instr::Jz(l) => format!("jz {l}"),
            Instr::Jnz(l) => format!("jnz {l}"),
            Instr::Jc(l) => format!("jc {l}"),
            Instr::Jo(l) => format!("jo {l}"),
            Instr::Jnc(l) => format!("jnc {l}"),
            Instr::Jmp(l) => format!("jmp {l}"),
            Instr::Cmovge(dest, src) => cmov_hack("cmovge", dest, src),
            Instr::Cmovle(dest, src) => cmov_hack("cmovle", dest, src),
            Instr::Cmovl(dest, src) =>  cmov_hack("cmovl", dest, src),
            Instr::Cmovg(dest, src) =>  cmov_hack("cmovg", dest, src),
            Instr::Cmovc(dest, src) =>  cmov_hack("cmovgc", dest, src),
            Instr::Cmovnc(dest, src) => cmov_hack("cmovnc", dest, src),
        }
    }
}

pub enum AssemblyLine {
    Instruction(Instr),
    Label(String),
}

impl Emit for AssemblyLine {
    fn emit(&self) -> String {
        use AssemblyLine::*;
        match self {
            Instruction(i) => "\t".to_string() + &i.emit(),
            Label(l) => format!("\n{l}:"),
        }
    }
}


