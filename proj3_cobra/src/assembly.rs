#[derive(Debug)]
pub enum Val {
    Reg(Reg),
    Imm(i32),
    RegOffset(Reg, i32),
}

#[derive(Debug)]
pub enum Reg {
    RAX,
    RBX,
    RSP,
}

#[derive(Debug)]
pub enum Instr {
    Mov(Val, Val),
    Add(Val, Val),
    Sub(Val, Val),
    Cmp(Val, Val),
    Test(Val, Val),

    Jne(String),
    Je(String),
}

pub enum AssemblyLine {
    Instruction(Instr),
    Label(String),
}

