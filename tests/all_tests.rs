mod infra;
// Your tests go here!
success_tests! {
    {
        name: fact,
        file: "fact.snek",
        input: "10",
        expected: "3628800",
    },
    {
        name: even_odd_1,
        file: "even_odd.snek",
        input: "10",
        expected: "10\ntrue\ntrue",
    },
    {
        name: even_odd_2,
        file: "even_odd.snek",
        input: "9",
        expected: "9\nfalse\nfalse",
    },
    {
        name: no_args,
        file: "no_args.snek",
        expected: "false\nfalse",
    },
    {
        name: many_prints,
        file: "many_prints.snek",
        expected: "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n21\n22\n23\n24\n25\n26\n27\n28\n29\n30\n31\n32\n33\n34\n35\n36\n37\n38\n39\n40\n41\n42\n43\n44\n45\n46\n47\n48\n49\n50\n51\n52\n53\n54\n55\n56\n57\n58\n59\n60\n61\n62\n63\n64\n65\n66\n67\n68\n69\n70\n71\n72\n73\n74\n75\n76\n77\n78\n79\n80\n81\n82\n83\n84\n85\n86\n87\n88\n89\n90\n91\n92\n93\n94\n95\n96\n97\n98\n99\n99"
    },

    {
        name: simple_vec,
        file: "simple_vec.snek",
        expected: "[1, 2, 3, 4]"
    },
    {
        name: vec_get,
        file: "vec_get.snek",
        expected: "995"
    },
    {
        name: vec_cycle1,
        file: "vec_cycle1.snek",
        expected: "[1, [...], 3]"
    },
    {
        name: vec_struct_eq,
        file: "vec_struct_eq.snek",
        expected: "true",
    },
    {
        name: vec_self_ref_struct_eq,
        file: "vec_self_ref_struct_eq.snek",
        expected: "false",
    },
    {
        name: vec_len,
        file: "vec_len.snek",
        expected: "5",
    },
    {
        name: vec_of_var,
        file: "vec_of_var.snek",
        expected: "[1, 2, 3, 4]"
    }
}

runtime_error_tests! {
    {
        name: vec_neg_idx,
        file: "vec_neg_idx.snek",
        expected: "",
    },
    {
        name: vec_get_not_vec,
        file: "error-tag.snek",
        expected: "",
    },
    {
        name: vec_get_oob,
        file: "vec_get_oob.snek",
        expected: "out of bounds"
    }
}

static_error_tests! {
    {
        name: duplicate_params,
        file: "duplicate_params.snek",
        expected: "",
    },
    {
        name: duplicate_fun,
        file: "duplicate_fun.snek",
        expected: "",
    },
    {
        name: fun_no_exist,
        file: "fun_no_exist.snek",
        expected: "",
    },
    {
        name: input_in_fun,
        file: "input_in_fun.snek",
        expected: "",
    },
    {
        name: fun_in_body,
        file: "fun_in_body.snek",
        expected: "",
    },
    {
        name: no_name_fun,
        file: "no_name_fun.snek",
        expected: "",
    },
    {
        name: wrong_arg_count,
        file: "wrong_arg_count.snek",
        expected: "",
    },
    {
        name: invalid_decl_no_body,
        file: "invalid_decl_no_body.snek",
        expected: "",
    },
}
