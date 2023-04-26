#[derive(Debug)]
pub enum Val {
    Reg(Reg),

    Imm(i64),

    /// stack index, will be multiplied by 8 to find actual position since we index the stack in
    /// 64-bit words
    StackIndex(usize)
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

