# (Green) Egg Eater Writeup

## Concrete Syntax Updates
My language includes all of the concrete syntax up through Diamondback.
Additionally, I have added the `vec`, `vec-get`, `vec-set!`, and `vec-len`
expressions. Additionally, I have added the `snek-vec-print` expression, which
will print a vector. I have added the `nil` type to represent an empty
heap-allocated value. Finally, I have also added one binary operator, `==`,
which implements structural equality checking.

```
<prog> := <defn>* <expr>                (new!)
<defn> := (fun (<name> <name>*) <expr>) (new!)
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
  | (vec <expr>+)
  | (vec-get <expr> <expr>)
  | (vec-set! <expr> <expr> <expr>)
  | (vec-len <expr>)
  | (loop <expr>)
  | (break <expr>)
  | (<name> <expr>*)                    (new!)

<op1> := add1 | sub1 | isnum | isbool | print (new!)
<op2> := + | - | * | < | > | >= | <= | = | ==

<binding> := (<identifier> <expr>)
```

## Behavior / Semantics
- `nil` evaluates to the nil vector
- the `vec` expression will create a heap-allocated vector with its length equal
    to the number of expressions passed to the `vec` expression, and evaluates
    to the newly-created vector
- the `vec-get <idx> <vector>` expression will attempt to get a value from a vector by
    evaluating the LHS then RHS. The LHS should evaluate to some index, and the
    RHS to some vector. 
- the `vec-set! <idx> <vector> <val>` expression will attempt to set a value in a vector by
    evaluating the argument expressions from left to right. The leftmost
    expression should evaluate to a vector, the middle should evaluate to some
    number, and the rightmost expression can evaluate to anything. If the index
    is within the bounds of the vector, then the value at `index` (zero-indexed)
    will be updated to `value`; otherwise, an error will be thrown. This
    expression will evaluate to the new value stored at `index`.
- the `vec-len <vector>` expression will evaluate to the number of items in the vector
- the `==` operator will check two values for structural equality. For numbers
    and booleans, this is the same as `=`; for vectors, this will traverse all
    elements in both vectors in a depth-first manner, looking for inequal
    values. It will evaluate to whether the vectors and all of their items are
    structurally equal to each other.

## Heap Diagram

todo, goes something like

```
---
length
val
val
val
...
val
---
length
val
val
...
val
---
length
val
```
etc

## Similarity to other heap-allocated languages
Check out python and java I guess.

## Outside Resources
I primarily used the Inkwell / LLVM Kaleidoscope tutorial to implement this
compiler. TODO add link

Additionally, here are a list of ChatGPT conversations that I had while
implementing this:
- todo
- 



