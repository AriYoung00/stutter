pub const EXIT_OPERAND_MISMATCH: &str = "exit_operand_mismatch";
pub const EXIT_EXPECTED_NUM: &str = "exit_expected_number";
pub const EXIT_EXPECTED_BOOL: &str = "exit_expected_bool";
pub const EXIT_OVERFLOW: &str = "exit_overflow";

pub type ParseResult<T> = Result<T, String>;
