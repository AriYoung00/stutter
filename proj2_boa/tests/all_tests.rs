mod infra;

// Your tests go here!
success_tests! {
    add1: "73",
    add: "15",
    binding: "5",
    binding2: "35",
    num: "132",
    let_expr: "420",
    nested_arith: "140",
}

failure_tests! {
    unbound_id: "Unbound variable identifier x",
    duplicate_binding: "Duplicate binding",
}
