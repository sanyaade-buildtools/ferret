# Ferret

## Introduction

Ferret is a functional, concatenative, symbolic language with roots in Forth, Haskell, and Erlang. It's 100% interpreted, but extremely fast. The most distinctive characteristic of Ferret is that it is a transactional language. This means that an entire block succeeds or the entire VM rolls back to its last, previously known, good state. In addition to being transactional, Ferret also supports the following features:

* Atoms (symbols)
* Light weight processes
* Actor messaging between processes
* Forth-like loops and conditionals
* Block closures 

## What Does Ferret Look Like?

Here are some simple examples of Ferret in action:

	"Hello, world!" puts

Mapping a block closure over a list of items:

	[1 2 3] {1 +} map

Define functions with block closures:

	: curry ( x xt -- xt' ) with x block -> {x block apply} ;
	: compose ( xt xt -- xt' ) with a b -> {a apply b apply } ;

Example test script:

	clone vm "ubuntu 11.04 64-bit" as node_1
	clone vm "ubuntu 11.04 64-bit" as node_2

	with node_1
		install "riak 1.1"
		start riak
	end

	with node_2
		install "riak 1.1"
		start riak
		join node_1
		stop riak    # want to test handoffs
	end

	with node_1
		put random data
	end

	with node_2
		start riak
	end

	# something here to test handoffs