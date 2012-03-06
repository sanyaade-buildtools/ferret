# Ferret Introduction

Ferret is a functional, concatenative, transactional language with roots in Forth, Haskell, and Erlang. It's interpreted, but extremely fast. Being transactional means that all code either succeeds or the entire VM rolls back to its last, previously known state. Ferret also has the following features:

* Atoms (symbols)
* Light weight processes
* Actor messaging between processes
* Forth-like loops and conditionals
* Block closures (with true lexical scoping)
* Local word definitions

# What Does Ferret Look Like?

Here are some simple examples of Ferret in action:

```
"Hello, world!" puts
```

Mapping a block closure over a list of items:

```
[1 2 3] {1+} map
```

Defining a function:

```
: len ( xs -- n ) 0 swap each 1+ next ;
```

# Fundamentals

Please note: this is **not** a tutorial on using a stack-based language and basic Forth primitives (`dup`, `drop`, …). There are plenty of excellent tutorials out there for working with stacks. Check out <http://www.forth.com/forth/forth-books.html> for some good books and reading materials.

## Dual Stacks

Like all other concatenative languages, Ferret is stack-based. There are two stacks in Ferret that you can work with:

* The parameter stack
* The control stack

### The Parameter Stack

The parameter stack is where 99% of all data you work on will be located. It functions as both inputs to functions and as the destination of return values. When there is no data on the stack, the prompt will be a simple `ok`, otherwise the top stack item will be displayed along with how many additional items are on the stack.

```
1 2
== 2 (+ 1)
```

You can display the contents of the stack using the `.s` function:

```
.s
[ +0 ] 2
[ +1 ] 1
== 2 (+ 1)
```

As you can see, the stack remained unchanged. If we call a function like `+` then we can see how it consumes parameters from the stack and returns back to the stack.

```
+
== 3
```

### The Control Stack

The control stack is a second, temporary stack that can be used quite effectively to help solve various problems. You can easily move data from the parameter stack to the control stack (and back) using the `push` and `pop` functions:

```
4
== 4
push
  ok
pop
== 4
```

If we use the `.s` function while there is data on the control stack, it can be visualized as data sitting "above" the parameter stack:

```
push
  ok
1 2
== 2 (+ 1)
.s
[ -1 ] 4
[ +0 ] 2
[ +1 ] 1
== 2 (+ 1)
clear
  ok
```

## Basic Types

Ferret has support for the following types:

* Atoms (symbols with O(1) comparison)
* Block closures
* Booleans
* Characters
* Lists
* Numbers (integer and float)
* Processes (pre-emptive, lightweight, actor-model ala Erlang)
* Strings

Here are some examples of these types:

```
Hello
== Hello
```

```
{10 dup = if "equal" puts then}
== {10 dup = if "equal" puts then}
```

```
T
== T
```

```
'c'
== 'c'
```

```
[1 2 3]
== [1 2 3]
```

```
103
== 103
```

```
12.e-3
== 0.012
```

```
{1 2 +} spawn
== <pid:1>
```

```
"Hello, world!"
== "Hello, world!"
```

## Lexical Environments

Ferret has support for lexical bindings. You can pop values off the stack into bindings using the `let` keyword.

```
10 let x -> x
== 10
```

We pushed 10 to the stack, then we created a new lexical environment in which we popped the top of the stack (10) and bound it to `x`. We then pushed the value of `x` in the body of the scope.

When popping values into a lexical environment, the right-most identifier denotes the top of the stack:

```
10 2 let x y -> x y **
== 100.
```

As you can see, `x` was bound to 10 and `y` was bound to 2 in the above example.

## Block Closures

Ferret also supports blocks, which are full, lexical block closures (they retain the environment they were created in). Blocks are created with the `{ .. }` syntax:

```
{"Hello" .}
== {"Hello" .}
```

Blocks are first-class values and can be passed around on the stack like any other object. When you want, you can `apply` a block to execute it (within the environment it was created in).

```
10 let x -> {x x * .} apply
100
  ok
```

## Local Definitions

Sometimes it's handy to create a word that is a helper, but isn't something you want accessible to the outside world. For this you can create a local word inside anther using `let:`.

```
let: say-hi ( -- ) "Hello, world!" . -> say-hi
Hello, world!
  ok
```

What's important here is that `say-hi` is only available within the scope it was defined in and not above. Additionally, local definitions can use the current lexical environment and be passed around in block closures.

```
: squared ( n -- xt ) let x -> let: sq ( -- n ) x x * -> {sq} ;
  ok
10 squared
== {sq}
apply
== 100
```

## Transactions

Ferret is a purely functional and *transactional* language. This means that there are never any side-effects from running Ferret code (outside of I/O), and all code either successfully completes or fails; you can never leave yourself in an unrecoverable state. To see this in action, let's try removing more items from the stack than are actually present.

```
1 2 3
== 3 (+ 2)
drop drop drop drop
** Stack underflow
== 3 (+ 2)
```

As you can see, when we reached the fourth `drop` call, the parameter stack underflowed and an exception was thrown. However, the system state was returned to the last known, good state before the code began executing. This gives us an opportunity to see determine what we did wrong, correct it, and continue working.

Being transactional allows for some very nice features, most of which center around a single function: `try`. The `try` function is like `apply`, except that it also returns `T` or `F` (the true/false values of Ferret) to indicate whether or not the block successfully completed.

```
{"hi" 2 +} try
== F
```

If the block failed to succeed, then it's as if none of the block executed at all, and the state is rolled back to immediately before the `try` call. You can use this behavior to write conditionals which manipulate the stack assuming everything will be okay, and if they fail, try a different branch:

```
: double ( x -- xx ) dup [{append} {&} {+}] case ;
  ok
"hi" double
== "hihi"
```

In the above example, the `case` function will `try` each block in-order, and the first one to succeed will exit the `case` function. The `append` function only works on lists, the `&` function concatenates two strings, and the `+` function adds two numbers.

## List Accumulation

While Ferret does support recursion just fine (via the `recurse` keyword), Ferret allows for lists to be built in a temporary parameter stack, and have the entire stack returned immediately as a list. This is all done under-the-hood for you:

```
[1 2 3]
== [1 2 3]
```

The above is *not* a literal list. Instead, inside the `[ .. ]` brackets a new parameter stack is created, 1, 2, and 3, are pushed onto it, and then the entire stack - as a list - is returned to the previous parameter stack.

Because of this, you can put arbitrary code inside `[ .. ]` to generate lists iteratively instead of recursively. For example, here is an example of generating a list of numbers:

```
[10 for i next]
== [9 8 7 6 5 4 3 2 1 0]
```

**Note: `for` loops count down instead of up in Ferret.**

You can see how easy it is to create list functions iteratively using list accumulation by taking a look at many of the extended functions in the `Lists` module:

```
-- generate a list of numbers from 1..n
: iota ( n -- xs ) let n -> [n for n i - next] ;

-- apply a block closure across the elements of a list
: map ( xs f -- ys ) let xs f -> [xs each i f apply next] ;

-- remove elements from a list that don't match a predicate
: filter ( xs p -- ys ) let xs p -> [xs each i p apply if i then next] ;
```

If you want to create a literal list in your code, you can use `#[ .. ]` to do so, in which case no accumulation will be performed and the list will be pushed as-is to the stack.

```
#[1 2 3]
== [1 2 3]
```

