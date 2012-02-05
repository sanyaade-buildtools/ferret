# Ferret

## Introduction

Ferret is a symbolic language with roots in Lisp and Erlang. It shamelessly shares the syntactic characteristics of REBOL, but that's where the similarity ends. Ferret is mutable, but the code is written in a purely-functional manner, which allows you to write 100% functional code, but use a few mutable data structures when it is advantageous to do so (ala OCaml).

Ferret is a pure interpreter. There is no bytecode and no compile step. Yet, it is remarkably fast. The goal of Ferret is to allow extremely fast prototyping and creation of useful tools without the hassle of a build step. Code is code, can be loaded and executed freely, whether it's from a file on disk, a remote site, or a user-input string.

## What Does Ferret Look Like?

Here are some simple examples of Ferret in action:

	print "Hello, world!"

Loading and executing remote code:

	do load read url http://www.ferret.com/games/guess.ferret

Sending an email to a list of people:

	emails: lines read %friends.txt

	foreach friend emails none [
		email friend {
			subject: "I'm loving Ferret!"
			body: "Check it out!"
		}
	]

