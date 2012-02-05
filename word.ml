(* ferret word
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * word.ml
 *)

(* binary operator *)
type op = Infix | Op

(* word type *)
type t =
    Sym of Atom.t
  | Binary of op * Atom.t
  | Getter of Atom.t
  | Var of Atom.t

(* extract the atom from a word *)
let atom = function
  | Sym x -> x
  | Binary (_,x) -> x
  | Getter x -> x
  | Var x -> x

(* compare words *)
let compare a b = Atom.compare (atom a) (atom b)
