(* canopy global symbol table
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * atom.ml
 *)

(* atom type *)
type t = { name : string
         ; i    : int
         }

(* alias for AtomMap *)
type atom = t

(* the one and only symbol table and current gensym counter *)
let symbols = Hashtbl.create 400
let gensym = ref 0

(* compare two atoms *)
let compare a b =
  match a.i = b.i with
      true  -> 0
    | false -> compare a.name b.name

(* map tree *)
module AtomMap = Map.Make(struct type t = atom let compare = compare end)

(* intern a new atom into the symbol table *)
let intern s =
  try
    Hashtbl.find symbols s
  with Not_found ->
    let sym = { name=s; i=(!gensym) } in
    Hashtbl.add symbols s sym;
    incr gensym;
    sym

(* ensure that an atom exists, don't create if not *)
let intern_existing = Hashtbl.find symbols

