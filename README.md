# Ferret

## Introduction

Ferret is a functional, concatenative, transactional language with roots in Forth, Haskell, and Erlang. It's interpreted, but extremely fast. Being transactional means that all code either succeeds or the entire VM rolls back to its last, previously known state. Ferret also has the following features:

* Atoms (symbols)
* Light weight processes
* Actor messaging between processes
* Forth-like loops and conditionals
* Block closures (with true lexical scoping)

## What Does Ferret Look Like?

Here are some simple examples of Ferret in action:

	"Hello, world!" puts

Mapping a block closure over a list of items:

	[1 2 3] {1+} map

Defining functions and lexical scopes:

	: adder ( n -- xt ) let n -> {n +} ;

Sum the squares of all numbers from [1..10]

	10 iota {dup *} map sum

Download Google's homepage:

	"http://www.google.com" url read

## Quickstart

### Fundamentals

If you have programmed Forth before (or another concatenative language), then much of this will seem familiar, but it is still recommended that you work through it, because there are differences.

#### Dual Stacks

Like all other concatenative languages, Ferret is stack-based. There are two stacks in Ferret:

* The parameter stack
* The control stack

##### The Parameter Stack

The parameter stack is where 99% of all data you work on will be located. It functions as both inputs to functions and as the destination of return values. When there is no data on the stack, the prompt will be a simple `ok`, otherwise the top stack item will be displayed along with how many additional items are on the stack.

```
1 2 3
== 3 (+ 2)
```

You can display the contents of the stack using the `.s` function:

```
.s
[ +0 ] 3
[ +1 ] 2
[ +2 ] 1
== 3 (+ 2)
```

As you can see, the stack remained unchanged. If we call a function like `+` then we can see how it consumes parameters from the stack and returns back to the stack.

```
+
== 5 (+ 1)
```

And we can use the `.` function to print the top stack item to `stdout`, which will consume and not return a value:

```
.
5
== 1
```

There is now only a single item left on the stack, which we can get rid of using the `drop` function.

```
drop
  ok
```

##### The Control Stack

The control stack serves two purposes: as a temporary place to store values and as a kind of monadic stack, allowing you to apply functions within a given context and/or state.

You can easily move data from the parameter stack to the control stack (and back) using the `push` and `pop` functions:

```
1
== 1
push
  ok
pop
== 1
```

We first put the value `1` onto the stack, then pushed it to the control stack, the popped it from the control stack back to the parameter stack again. If we use the `.s` function while there is data on the control stack, we can visualize it:

```
push
  ok
.s
[ -1] 1
Empty Stack
  ok
```

When you display the stack, negative stack indices are on the control stack; -1 is the top of the control stack.

Along with `push` and `pop`, there are a few other functions that allow you to use the control stack like a state monad. You can `get` the top of the control stack, replace it with `put`, execute a block with `do`, and `lift` another block into the previous control stack state.

```
1 {get .} do
1
== 1
```

The above code, when `do` was called, pushed `1` onto the control stack, then applied the block `{get .}`, which got the current control stack top value (`1`), printed it (`.`), and then exited the block, popping the control stack.