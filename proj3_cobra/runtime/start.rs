use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: i64) -> u64;
}

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    // TODO: print error message according to writeup
    eprintln!("{}", match errcode {
        7 => "invalid argument - expected number, not bool",
        8 => "invalid argument - expected bool, not number",
        9 => "error - arithmetic overflow",
        _ => "an unknown error occurred",
    });
    std::process::exit(1);
}

fn parse_input(input: &str) -> i64 {
    // TODO: parse the input string into internal value representation
    if input == "true" {
        0b11
    }
    else if input == "false" {
        0b01
    }
    else {
        let val: i64 = input.parse().expect("invalid argument");
        if val > 4611686018427387903 || val < -4611686018427387904 {
            panic!("invalid argument")
        }

        let preserve_sign_mask = val & (1 << 63);
        let shifted = val << 1;

        shifted | preserve_sign_mask
    }
}

fn main() {
    assert_eq!(parse_input("0"), 0);
    assert_eq!(parse_input("1"), 0b010);
    assert_eq!(parse_input("2"), 0b100);
    assert_eq!(parse_input("3"), 0b110);

    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    if i == 3 {
        println!("true");
    }
    else if i == 1 {
        println!("false");
    }
    else {
        let n = i >> 1;
        println!("{n}");
    }
}
