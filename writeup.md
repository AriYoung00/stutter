# (Green) Egg Eater Writeup

## Implementation Note
I implemented my initial Egg Eater, and this extension, as an LLVM frontend
rather than emitting actual assembly. I did this using [the Inkwell rust
crate](https://github.com/TheDan64/inkwell). Consequently, in order to compile
my code, the LLVM 15 libraries and header files must be installed on your system:
```shell
# MacOS / Homebrew
brew install llvm@15 && brew link llvm@15
# Fedora / CentOS
sudo dnf install llvm15-devel
# Ubuntu / maybe Debian? idk
sudo apt install llvm-15-dev
```
They must also be made available to whichever linker `rustc` uses.


## Concrete Syntax Updates
My language includes all of the concrete syntax up through Diamondback.
Additionally, I have added the `vec`, `vec-get`, `vec-set!`, and `vec-len`
expressions. Additionally, I have added the `snek-vec-print` expression, which
will print a vector. I have added the `nil` type to represent an empty
heap-allocated value. Finally, I have also added one binary operator, `==`,
which implements structural equality checking.

```
<prog> := <defn>* <expr>
<defn> := (fun (<name> <name>*) <expr>)
<expr> :=
  | <number>
  | true
  | false
  | nil
  | input
  | <identifier>
  | (let (<binding>+) <expr>)
  | (<op1> <expr>)
  | (<op2> <expr> <expr>)
  | (set! <name> <expr>)
  | (if <expr> <expr> <expr>)
  | (block <expr>+)
  | (vec <expr>+) (new)
  | (vec-get <expr> <expr>) (new)
  | (vec-set! <expr> <expr> <expr>) (new)
  | (vec-len <expr>) (new)
  | (loop <expr>)
  | (break <expr>)
  | (<name> <expr>*)

<op1> := add1 | sub1 | isnum | isbool | print
<op2> := + | - | * | < | > | >= | <= | = | ==

<binding> := (<identifier> <expr>)
```

## Behavior / Semantics
Note -- I have only outlined semantics which have changed since Diamondback.

- `nil` evaluates to the nil vector
- `(vec arg1 ... argN)` will allocate a new vector of size N on the heap, with
    contents `[arg1, arg2, ..., argN]` (same as Forest Flame starter).
- `(vec-get <idx> <vector>)` will attempt to get a value from a vector by
    evaluating the LHS then RHS. The LHS should evaluate to some index, and the
    RHS to some vector. This expression is zero-indexed: `(vec-get 0 <vector>)`
    will return the first element in `<vector>`.
- `(vec-set! <idx> <vector> <val>)` will attempt to set a value in a vector by
    evaluating the argument expressions from left to right. The leftmost
    expression should evaluate to some number, the middle should evaluate to a
    vector, and the rightmost expression can evaluate to anything. If the index
    is within the bounds of the vector, then the value at `idx` (zero-indexed)
    will be updated to `value`; otherwise, an error will be thrown. This
    expression will evaluate to the new value stored at `index`.
- (`vec-len <vector>` expression will evaluate to the number of items stored in
    `<vector>`.
- The `==` operator will check two values for structural equality. For numbers
    and booleans, this is the same as `=`; for vectors, this will traverse all
    elements in both vectors in a depth-first manner, looking for unequal
    values. It will evaluate to whether the vectors and all of their items are
    structurally equal to each other.

## Heap Diagram

My heap is laid out as follows. In this diagram, lesser addresses are at the
top, and as we move towards the bottom addresses increase. The `---` separators
serve as a visual reference to the reader indicating where the boundary between
different vectors is, and they are not actually represented in memory. Each line
which does not contains a `---` represents one word (i.e. 64 bits) of memory.

```
HEAP START
---
<length>
<value>
<value>
...
<value>
---
<length>
<value>
<value>
...
<value>
---
.
.
.
```
Note that these lenghts are represented in memory as "real" integers, and not as
tagged Snek integers.


## Tests
### `simple_examples.snek`
**Code**
```
(fun (fst v) (vec-get 0 v))
(fun (snd v) (vec-get 1 v))

(block
    (print (vec-get (+ 1 1) (vec 4 3 995 1)))


    (let ((a (vec 1 2 3)))
        (block
            (vec-set! 2 a 99)
            (print a)))


    (let ((a (vec 36 4)) (b (vec a)) (c (vec b)) (d (vec c)))
        (print (fst (fst (fst (fst d))))))


    (let ((a (vec nil 50 60)) (b (vec nil 70 80)))
        (block
            (vec-set! 0 a b)
            (vec-set! 0 b a)
            (print (snd (fst (fst (fst (fst (fst (fst a)))))))))))
```
**Output**
```
995
[1, 2, 99]
36
50
50
```

### `error-tag.snek`
**Code**
```
(+ 1 (vec 1 2 3))
```
**Output**
```
CSE131_Projects on  feature-emit-llvm [!] is  v0.1.0 via 🦀 v1.68.0-nightly 
at 23:02:19 💥 input/error-tag.run
invalid argument - expected number
```
This error is caught by a runtime type check. This is a snippet of LLVM IR that
is generated inline with the program -- it is not strictly speaking part of the
runtime. In this case, when I call `+`, it will check to make sure both operands
are numbers, and exit with an error if they are not.

### `error-bounds.snek`
**Code**
```
(let ((a (vec 1 2 3 4 5)))
    (vec-get 5 a))
```
**Output**
```
CSE131_Projects on  feature-emit-llvm [!] is  v0.1.0 via 🦀 v1.68.0-nightly 
at 23:01:31 💥 input/error-bounds.run
vec index out of bounds
```
This error is caught by my runtime bounds check. Each time we attempt to access
a vector, my compiler will generate a bounds check inline using LLVM IR. It
ensures that, after the `index` expression is evaluated, the returned value is
both a number and within the range `[0, (vec-len <vector>))`, where the
beginning of the range is inclusive and the end is exclusive.


### `error3.snek`
**Code**
```
(vec-get 0 nil)
```
**Terminal**
```
CSE131_Projects on  feature-emit-llvm [!] is  v0.1.0 via 🦀 v1.68.0-nightly 
at 23:00:16   input/error3.run
invalue argument - expected vec
```
This error is also caught by my runtime type check. Strictly speaking, it is not
an error which is caught using tagging -- since `nil` and vectors have the same
tag, it is not caught by a tag check. However, I found it easier to treat `nil`
as its own type, where it is defined by having a `vector` tag but with its upper
bits zeroed. Thus, this is caught by my runtime type check (not tag check), and
is related to heap-allocated values.


### `points.snek`
**Code**
```
(fun (point x y) (vec x y))

(fun (point_add p1 p2)
    (vec 
        (+ (fst p1) (fst p2))
        (+ (snd p1) (snd p2))))

(fun (fst p) (vec-get 0 p))
(fun (snd p) (vec-get 1 p))

(block
    (let ((a (point 1 2)) (b (point 3 4)))
        (print (point_add a b)))

    (print (point_add
        (point 1 4)
        (point -1 -4)))

    (print (point_add
        (point -1 4)
        (point 1 -4)))

    (let ((p (point 365 0))) 
        (loop
            (if (> (fst p) 0)
                (set! p (point_add p (point -1 1)))
                (break p)))))
```
**Output**
```
[4, 6]
[0, 0]
[0, 0]
[0, 365]
```

### `bst.snek`
**Code**
```
(fun (node value a b) (vec value a b))


(fun (get_val n) (vec-get 0 n))
(fun (set_val n value) (vec-set! 0 n value))


(fun (left n) (vec-get 1 n))
(fun (right n) (vec-get 2 n))


(fun (set_left n sub) (vec-set! 1 n sub))
(fun (set_right n sub) (vec-set! 2 n sub))


(fun (make_bst) (vec nil nil nil))


(fun (insert n value)
    (if (== (get_val n) nil)
        (block
            (set_val n value)
            (set_left n (make_bst))
            (set_right n (make_bst))
            n)

        (if (> value (get_val n))
            (insert (right n) value)
            (insert (left n)  value))))


(fun (contains tree value)
    (if (== (get_val tree) value)
        true

        (if (== (get_val tree) nil)
            false

            (if (> value (get_val tree))
                (contains (right tree) value)
                (contains (left tree) value)))))


(let ((the_tree (make_bst)) (another_tree (make_bst)))
    (block
        (insert the_tree 5)
        (insert the_tree 4)
        (insert the_tree 6)
        (insert the_tree -1)
        (insert the_tree 999)
        (insert the_tree 998)
        (print (contains the_tree 999))
        (print (contains the_tree 997))
        (print the_tree)
        
        (insert another_tree 500)
        (insert another_tree 498)
        (insert another_tree 502)
        (insert another_tree 499)
        (insert another_tree 501)
        (print another_tree)
        ))
```
**Output**
```
true
false
[5, [4, [-1, [nil, nil, nil], [nil, nil, nil]], [nil, nil, nil]], [6, [nil, nil, nil], [999, [998, [nil, nil, nil], [nil, nil, nil]], [nil, nil, nil]]]]
[500, [498, [nil, nil, nil], [499, [nil, nil, nil], [nil, nil, nil]]], [502, [501, [nil, nil, nil], [nil, nil, nil]], [nil, nil, nil]]]
[500, [498, [nil, nil, nil], [499, [nil, nil, nil], [nil, nil, nil]]], [502, [501, [nil, nil, nil], [nil, nil, nil]], [nil, nil, nil]]]
```
In my BST implementation, I chose to represent empty nodes as `[nil, nil, nil]`.
Nodes are defined as a `(vec contents left_subtree right_subtree)`. 


## Similarity to other heap-allocated languages
### C
My implementation of heap-allocation is not very similar to C at all. In my Snek
implementation, heap-allocated values are stored with a length-prefix -- when
allocating a heap value of size `n`, we will reserve `n+1` words on the heap.
The first word will store the length of the vector, and the rest of the words
will store the vector's contents.

Conversely, in C, all heap allocations are left up to the programmer.
Heap-allocated blocks of memory contain no information about their length, as
the programmer is expected to keep track of this information. This has been the
source of many bugs (understatement of the century) over time.

Since C is statically typed, its pointers are also not tagged. This is
relatively dissimilar to my implementation, since my vec values contain a tag
indicating that they are heap-allocated memory.

### Python
My implementation of heap-allocation is reasonably similar to that of lists in
Python. In Python, lists are stored as references to heap-allocated arrays. I
was unable to find any information on whether the length of each list is stored
in the stack or the heap in python, but lists are certainly length-tagged, which
is also similar to my implementation of `vec` -- we always know how long a chunk
of heap-allocated memory is. Additionally, Python tags its references, rather
than allowing for raw pointers -- this is also similar to my implementation.

## Outside Resources
I primarily used the Inkwell / LLVM Kaleidoscope tutorial to implement this
compiler. TODO add link

I also found some information about how python stores lists at [this link](https://www.opensourceforu.com/2021/05/memory-management-in-lists-and-tuples/)

I also collaborated with Timothy Bodrov (another CSE131/231 student) in order to
determine the best way to cyclic structural equality. He also gave me some
feedback on my implementation of BSTs in Snek.


## Structural Equality
### Implementation
I implemented the structural equality operator as a simple call to a runtime
function. This allows us to draw meaningful conclusions about cyclic equality,
something which would be far too complex to implement in assembly (or in my
case, LLVM IR).

### Cyclic Behavior
In the presence of a cycle, the structural equality binary operator will attempt
to mimic the behavior of Racket by characterizing the expansion to infinity of
the cycle.

When the structural equality operator finds two references at the same position
in two vectors which were equal up until that point, it will assume that the
rest of the vectors are equal. Then, it will traverse the graph formed by the
aforementioned references until it reaches a pair of references which it has
already seen, at which point it will assume that the entire cycle is equal.
Then, it will traverse the rest of the initial vectors (and also any other
vectors present in the graph) searching for an inequality. If it finds none,
then it knows that it was initially correct in assuming that the cycles were
equal. If it finds some difference in "matching" nodes in the two cycles, then
it will immediately short circuit, and the whole structural equality check will
return false.

For illustration, consider the following Rust snippet from my runtime, which
impements structural equality of vectors:
```rust
unsafe fn snek_struct_eq_impl(lhs: u64, rhs: u64, seen: &mut HashSet<(*const u64, *const u64)>) -> bool {
    (other code elided...)
    // vectors
    if (lhs & 0b1) == 0b1 {
        // short circuit for nil
        if (lhs == 0b1) || (rhs == 0b1) {
            return lhs == rhs;
        }

        let lhs_ptr = (lhs - 1) as *const u64;
        let rhs_ptr = (rhs - 1) as *const u64;

        // if we've attempted to check equality on these two vectors before
        // assume they are self-referential in some complementary way
        // since this means we've fully traversed two "complementary" cycles in lhs and rhs, this
        // means that we have been unable to find some concrete difference between the cycles
        // therefore they must be equal
        if seen.contains(&(lhs_ptr, rhs_ptr)) || seen.contains(&(rhs_ptr, lhs_ptr)) {
            // fall back to referential equality
            return true;
        }
        seen.insert((lhs_ptr, rhs_ptr));

        let lhs_len = *lhs_ptr;
        let rhs_len = *rhs_ptr;

        if lhs_len != rhs_len {
            return false;
        }

        for i in 0_isize..(lhs_len as isize) {
            let lhs_val = *lhs_ptr.offset(i+1);
            let rhs_val = *rhs_ptr.offset(i+1);

            if snek_struct_eq_impl(lhs_val, rhs_val, seen) == false {
                return false;
            }
        }

        seen.remove(&(lhs_ptr, rhs_ptr));
        return true;
    }
}
```
We can see that, as soon as it begins checking the equality of two vectors, it
inserts them into the `seen` set -- this constitutes the assumption that the
entirety of the vectors are equal. Then it checks the lengths and referential
equality of the vectors, short circuiting in the appropriate manner in both
cases. Then, it traverses the vectors in parallel in a depth-first fashion,
immediately recursing when it finds another reference. At any point, if it
encounters a reference which it has already seen, it knows that it has found a
cycle, and that all of the vectors which make up the cycle are equal up until
those references which actually form the cycle. 

After closing the cycle, it will then check the remainder of all the vectors
which make up the cycle, after the references which form the cycle. If it finds
any inequalities at this point, it knows that the assumption that the cycles
were equal was incorrect, and `return false` will short-circuit all the way back
up the call stack. Otherwise, it will return `true`, since it has been able to
show that the initial assumption was correct.


## Cyclic Printing
The manner in which my implementation handles cyclic printing is very simple --
it performs a depth-first traversal of all stack-allocated references, keeping
track of those which have already been seen. If it encounters a reference to a
vector which has been seen, it knows that it has found a cycle, and does not
traverse it any further, simply printing an ellipses: `[...]`. This matches the
behavior of Python. See the following code for reference:
```rust
unsafe fn snek_val_to_str(val: u64, seen: &mut HashSet<*const u64>) -> String {
    match val {
        0b011 => "false".into(),
        0b111 => "true".into(),
        0b001 => "nil".into(),
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
```
The `snek_val_to_str` function is called and its result is directly printed by
`snek_print`.

## New Tests

### `equal.snek`
**Code**
```
(block
    (let ((a (vec 1 2 3 4 55 60)) (b (vec 1 2 3 4 55 60)))
        (block 
            (print (= a b))
            (print (== a b))))

    (let ((a (vec 1 2 3)) (b (vec a 5 6)) (c (vec 1 2 3)) (d (vec c 5 6)))
        (print (== b d)))

    (let ((a (vec 1 2 3)))
        (print (== a a)))


    (let ((a (vec 1 2 3 4 55)) (b (vec 1 2 3 4 56)))
        (print (== a b)))

    (let ((a (vec 1 2 4)) (b (vec a 5 6)) (c (vec 1 2 3)) (d (vec c 5 6)))
        (print (== b d)))

    (let ((a (vec 1 2 4)) (b (vec a 5 6)) (d (vec a 5 7)))
        (print (== b d)))
)
```
**Output**
```
false
true
true
true
false
false
false
false
```


### `cycle-print-1.snek`
**Code**
```
(let ((a (vec 1 2 3)))
    (block
        (vec-set! 1 a a)
        a))
```
**Output**
```
[1, [...], 3]
```

### `cycle-print-2.snek`
**Code**
```
(let ((a (vec 1 2 3)) (b (vec 4 5 6)))
    (block
        (vec-set! 1 a b)
        (vec-set! 1 b a)
        a))
```
**Output**
```
[1, [4, [...], 6], 3]
```

### `cycle-print-3.snek`
**Code**
```
(let ((a (vec 1 2 3)) (b (vec 4 5 6)) (c (vec 7 8 9)))
    (block
        (vec-set! 1 a b)
        (vec-set! 1 b c)
        (vec-set! 1 c a)
        a))
```
**Output**
```
[1, [4, [7, [...], 9], 6], 3]
```

### `cycle-equal-1.snek`
**Code**
```
(let (
    (a (vec 1 nil 2))
    (b (vec 3 4 nil))
    (c (vec 1 nil 2))
    (d (vec 1 nil 2))
    (e (vec 3 4 nil)))

    (block
        (vec-set! 1 a b)
        (vec-set! 2 b c)
        (vec-set! 1 c b)
        (vec-set! 1 d e)
        (vec-set! 2 e d)
        (print a)
        (print d)
        (== a d)))
```
**Output**
```
[1, [3, 4, [1, [...], 2]], 2]
[1, [3, 4, [...]], 2]
true
```

### `cycle-equal-2.snek`
**Code**
```
(let (
    (a (vec 1 2 nil))
    (b (vec 1 2 nil)))

    (block
        (vec-set! 2 a a)
        (vec-set! 2 b b)
        (print a)
        (print b)
        (== a b)))
```
**Output**
```
[1, 2, [...]]
[1, 2, [...]]
true
```

### `cycle-equal-3.snek`
**Code**
```
(let (
    (a (vec 1 nil 2))
    (b (vec 3 4 nil))
    (c (vec 3 4 nil))
    (d (vec 1 nil 2)))

    (block
        (vec-set! 1 a b)
        (vec-set! 2 b a)
        (vec-set! 2 c d)
        (vec-set! 1 d c)
        (print a)
        (print c)
        (== a c)))
```
**Output**
```
[1, [3, 4, [...]], 2]
[3, 4, [1, [...], 2]]
false
```

