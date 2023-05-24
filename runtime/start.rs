use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: i64, heap_top: *mut u64) -> i64;
}

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    // TODO: print error message according to writeup
    eprintln!("{}", match errcode {
        7 => "invalid argument - expected number",
        8 => "invalid argument - expected bool",
        9 => "error - arithmetic overflow",
        10 => "invalid argument(s) - mismatched operand types",
        11 => "invalue argument - expected tuple",
        _ => "an unknown error occurred",
    });
    std::process::exit(1);
}

#[export_name = "\x01snek_print"]
pub extern "C" fn snek_print(val: i64) {
    match val {
        0b011 => println!("false"),
        0b111 => println!("true"),
        _ if (val & 0b01) == 1 => {
            println!("print tuple is not yet implemented")
        },
        _ => println!("{}", val >> 1),
    };
}

fn parse_input(input: &str) -> i64 {
    if input == "true" {
        0b111 // 0b111 = true
    }
    else if input == "false" {
        0b011 // 0b011 = false
    }
    else {
        let val: i64 = input.parse().expect("invalid argument");
        if val > 4611686018427387903 || val < -4611686018427387904 {
            panic!("invalid argument")
        }
        val << 1
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

    let mut memory = Vec::with_capacity(100_000);
    let buf: *mut u64 = memory.as_mut_ptr();

    let i: i64 = unsafe { our_code_starts_here(input, buf) };
    if i == 0b111 {
        println!("true");
    }
    else if i == 0b011 {
        println!("false");
    }
    else if (i & 0b01) != 0 {
        println!("tuple value");
    }
    else {
        let n = i >> 1;
        println!("{n}");
    }
}
