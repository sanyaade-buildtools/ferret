# Ferret

## Introduction

Ferret is a symbolic language with roots in Lisp and other functional languages. It shamelessly shares the syntactic characteristics of REBOL, and a few concepts, but that's where the similarity ends.

Ferret is a pure interpreter. There is no bytecode and no compile step. Yet, it is remarkably fast. The goal of Ferret is to allow extremely fast prototyping and creation of useful tools without the hassle of a build step. 

Ferret attempts to be extremely high-level and trivialize many internet-based operations like sending emails, loading URLs, etc. It also means to be extremely reflective. Inspecting a value produces readable strings that can be used to load the object again. This means that code is data and can be loaded/saved trivially.

## What Does Ferret Look Like?

Here are some simple examples of Ferret in action:

	print "Hello, world!"

Doubling a list of numbers:

	map (fn [x] [x * 2]) [1 2 3]

Apply a function to a list of arguments:

	apply :+ [1 2]

Making a mutable objet context:

	user: make object [
		name: "riak"
		address: email "riak@basho.com"
	]

Loading and executing remote code:

	load url "http://www.mysite.com/games/guess.fsh"



