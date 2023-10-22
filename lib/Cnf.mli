module type CLAUSE =
  sig
    type lit = bool * string
    type t = lit list
    val add_lit : lit -> t -> t
    val del_lit : lit -> t -> t
    val empty : unit -> t
    val from_string : string -> t
    val pp_clause : t -> string
  end
module Clause : CLAUSE
module type CNF =
  sig
    type t
    val from_dimacs : string -> t
    val to_dimacs : t -> (string, int) Hashtbl.t * string
    val add_clause : Clause.t -> t -> t
    val stats : t -> int * int
    val empty : unit -> t
  end
module Cnf : CNF