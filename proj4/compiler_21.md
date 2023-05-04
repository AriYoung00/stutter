# Compiler 21 Review

## Sample Programs

### Test Program 1: Factorial
This test program calculates the factorial of the input number. Here is the
program:
```
(let ((x input) (acc 1))
    (loop
        (if (= x 0)
            (break acc)
            (block
                (set! acc (* acc x))
                (set! x (sub1 x))
            )
        )
    )
)
```
This test program features two different binary operators (`+` and `*`), as well
as an `input` expression and a loop that runs many times, depending on user
input.

#### Code Snippet 1
```rust
        Expr::Loop(e) => {
            let startloop = new_label(l, "loop");
            let endloop = new_label(l, "loopend");
            v.push(Instr::LABEL(startloop.to_string()));
            compile_expr(&e, v, env, si, &endloop, l);
            v.push(Instr::JMP(startloop.to_string()));
            v.push(Instr::LABEL(endloop.to_string()));
        }
        Expr::Break(e) => {
            if brake.is_empty() {
                panic!("break")
            }
            compile_expr(&e, v, env, si, brake, l);
            v.push(Instr::JMP(brake.to_string()));
        }
```
This code snippet relates to my first test program because it handles loop-based
control flow. 


#### Code Snippet 2
```rust
enum Instr {
...
    LABEL(String),
...
}
```
I have elided most of the `Instr` enum to highlight what I find interesting
about this snippet. This snippet is relevant to my test program because it
pertains to how the author represents labels internally, which are necessary to implement
all of the control flow present in my test program.

#### Code Snippet 3
```rust
                Op1::Sub1 => {
                    v.push(Instr::TEST(Val::Reg(Reg::RAX), Val::Imm(1))); // need this if assignment operators contains both
                    v.push(Instr::JNZ("throw_error_i".to_string()));
                    v.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(2)));
                    v.push(Instr::JO());
                }
```
This code snippet is (obviously) the implementation of `sub1`, which is relevant
because I use `sub1` in my test program. I think it is interesting because it
shows the implementation of the runtime numerical type check that this author
used. It is different from mine in that it uses the `test` instruction with `0b1`.
Conversely, in my compiler, I chose to use the `bt` instruction to directly move
the tag bit into the carry register and jump based on the carry register's
value. In this case, I believe that neither implementation is superior -- just
different ways to accomplish the same thing.


### Test Program 2
```
(let ((x 0))
    (loop
        (block 
            (if (isbool x)
                (break 1)
                (set! x (+ 1 x))
            )
            (if (< x 0)
                (break 2)
                (set! x (* 1 x))
            )
        )
    )
)
```
This test is slightly different, in that if it is running correctly, it should
not exit (realistically, unless left to run for a very long time). The point of
this code is to attempt to fuss out edge cases / unintended behavior with the
author's tagging implementation by repeatedly adding to and trivially
multiplying a numberic value, each iteration checking to ensure that it hasn't
somehow become a boolean. For this compiler, I have not observed it exiting,
implying that the implementation has no easily-reachable defects or edge cases.

#### Code Snippet 1
```rust
                    Op2::Times => {
                        v.push(Instr::SAR(Val::Reg(Reg::RAX)));
                        v.push(Instr::IMul(
                            Val::Reg(Reg::RAX),
                            Val::RegOffset(Reg::RSP, si * 8),
                        ));
                        v.push(Instr::JO());
                    }
```
This is the author's implementation of the `*` binary operator. It is relevant
to my second test program because my second test program repeatedly multiplies a
value by 1 using the binary operator `*`. More generally, it is related to my
test program because it showcases the simple implementation of `*` (this is a
good thing) which contains no bugs or edge cases which are obvious to me.


#### Code Snippet 2
```rust
                Op1::IsBool => {
                    let end_label = new_label(l, "ifend");
                    let else_label = new_label(l, "ifelse");
                    v.push(Instr::XOR(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::TEST(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::JNZ(else_label.to_string()));
                    v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                    v.push(Instr::JMP(end_label.to_string()));
                    v.push(Instr::LABEL(else_label.to_string()));
                    v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    v.push(Instr::LABEL(end_label.to_string()));
                }
```
This is the author's implementation of the `isbool` unary operator. It is
relevant to my second test program because my second test program repeatedly
calls this operator to detect if an incorrect type change has occurred. This
snippet is interesting to be because the author has implemented the
functionality using jumps and a conditional, rather than a simple conditional
move into `RAX` (as is used elsewhere in compiler 21).


#### Code Snippet 3
```rust
                    Op2::LessEqual => {
                        let end_label = new_label(l, "ifend");
                        let else_label = new_label(l, "ifelse");
                        v.push(Instr::CMP(
                            Val::Reg(Reg::RAX),
                            Val::RegOffset(Reg::RSP, si * 8),
                        ));
                        v.push(Instr::JLE(else_label.to_string()));
                        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                        v.push(Instr::JMP(end_label.to_string()));
                        v.push(Instr::LABEL(else_label.to_string()));
                        v.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                        v.push(Instr::LABEL(end_label.to_string()));
                    }
```
This is the author's implementation of the `<=` binary operator. It is relevant
to my second test program because my second test program uses it to check
whether or not any incorrect integer overflow / wraparound has occurred, which
would not be correct behavior.


## Bugs, Missing Features, Design Decisions

I have been unable to find any examples of programs which do not execute
correctly when compiled using compiler_21, aside from one small parsing bug.
This compiler does not check for number overflow in compiled constants properly.
It accepts the number 4611686018427387904 > 4611686018427387903, which is out of
bounds for an i63, as a valid program. This should be simple to fix, however --
some additional logic in `compile_expr` under the `Expr::number` match case
should be enough to fix this bug. Besides this, I believe that compiler_21
correctly implements Cobra. I feel confident about this because it passes my own
tests, in addition to all of the autograder tests and the (extremely!) comprehensive 
test suite implemented by compiler_57.


# Lessons and Advice

*Answer the following questions:*

    Identify a decision made in this compiler that's different from yours. Describe one way in which it's a better design decision than you made.
    Identify a decision made in this compiler that's different from yours. Describe one way in which it's a worse design decision than you made.
    What's one improvement you'll make to your compiler based on seeing this one?
    What's one improvement you recommend this author makes to their compiler based on reviewing it?

1. One thing that I liked about this compiler was that the author directly
    included the most recent loop exit label in the compilation context (see
    sample program 1, snippet 1). Comparatively, my implementation passes a
    "label index" corresponding to the current loop's exit label, rather than
    directly passing down the label. Overall, I think this author's approach is
    more flexible, and therefore better.
2. One thing that I like better about my compiler is the internal representation
    of assembly labels (see: sample program 1, code snippet 2). The author of
    this compiler has chosen to represent labels as a type of instruction, which
    I find interesting -- in my compiler, I chose to wrap the `Instr` enum inside
    of another enum: 
    ```rust
        enum AssemblyLine {
            Instructure(Instr), 
            Label(String)
        } 
    ```
    In this case, I believe that my implementation is overall better. Although
    it leads to a slightly messier codebase, it is more explicit and could
    possibly prevent certain bugs through the Rust type checker.

4. One improvement which I will make to my compiler after seeing this one is my
    implementation of the `*` binary operator. My implementation does not shift
    one operand prior to multiplying, which leads to error in the result by a
    factor of two. It is a simple mistake, but I did not realize I was making it
    until I read this compiler's implementation of `*`.
5. One improvement that I would recommend to this author after reading their
    compiler is to move more of their code into functions. Essentially all of the
    compilation logic is present inside of one giant `match...` within
    `compile_expr`, and this makes it a bit hard to read and follow the specific
    control flow. I believe that refactoring much of this code into specific
    functions would make it both more readable and more maintainable /
    extensible.

