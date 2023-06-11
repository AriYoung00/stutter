use std::env;
use std::collections::HashSet;

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
        12 => "tuple index out of bounds",
        _ => "an unknown error occurred",
    });
    std::process::exit(1);
}

#[export_name = "\x01snek_print"]
pub extern "C" fn snek_print(val: u64) {
    let mut seen = HashSet::new();
    println!("{}", unsafe { snek_val_to_str(val, &mut seen) });
}

#[export_name = "\x01snek_struct_eq"]
pub extern "C" fn snek_struct_eq(lhs: u64, rhs: u64) -> u64 {
    let res = unsafe { snek_struct_eq_impl(lhs, rhs) };
    if res {
        0b111
    }
    else {
        0b011
    }
}

unsafe fn snek_struct_eq_impl(lhs: u64, rhs: u64, seen: &mut HashSet<(*const u64, *const u64)>) -> bool {
    // check tags and lower bit if integer
    if (lhs & 0b11 != rhs & 0b11) {
        return false;
    }
    // if we're here, they must be the same type

    // integers
    if (lhs & 0b1 == 0) {
        return lhs == rhs;
    }
    // booleans
    if (lhs & 0b11 == 0b11) {
        return lhs == rhs;
    }

    // vectors
    if (lhs & 0b1 == 0b1) {
        let lhs_ptr = (lhs - 1) as *const u64;
        let rhs_ptr = (rhs - 1) as *const u64;

        // if we've attempted to check equality on these two vectors before
        // assume they are self-referential in some complementary way and return true
        if seen.contains((&lhs_ptr, &rhs_ptr)) || seen.contains((&rhs_ptr, &lhs_ptr)) {
            return true;
        }
        seen.insert((lhs_ptr, rhs_ptr));
        
        let lhs_len = *lhs_ptr;
        let rhs_len = *rhs_ptr;

        if (lhs_len != rhs_len) {
            return false;
        }

        for i in 0_isize..(lhs_len as isize) {
            let lhs_val = *lhs_ptr.offset(i+1);
            let rhs_val = *rhs_ptr.offset(i+1);

            if (snek_struct_eq_impl(lhs_val, rhs_val, seen) == false) {
                return false;
            }
        }
        return true;
    }

    // otherwise wtf is happening
    panic!("unrecognized type passed to struct_eq");
}

unsafe fn snek_val_to_str(val: u64, seen: &mut HashSet<*const u64>) -> String {
    match val {
        0b011 => "false".into(),
        0b111 => "true".into(),
        _ if (val & 0b01) == 1 => {
            let vec_start = (val - 1) as *const u64;

            if seen.contains(&vec_start) {
                return "[...]".into()
            }
            seen.insert(vec_start);

            let len: isize = (*vec_start) as isize;
            let mut subs: Vec<String> = Vec::new();
            for i in 0_isize..len {
                subs.push(snek_val_to_str(*vec_start.offset(i+1), seen));
            }
            seen.remove(&vec_start);

            format!("[{}]", subs.join(", "))
        },
        _ => format!("{}", (val as i64) >> 1),
    }
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

    let i: u64 = unsafe { our_code_starts_here(input, buf) } as u64;
    let mut seen = HashSet::new();
    let str_res = unsafe{ snek_val_to_str(i, &mut seen) };
    println!("{}", str_res);
}
