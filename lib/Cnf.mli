module type CLAUSE = sig
  type lit = bool * string
  type t = lit list

  val add_lit : lit -> t -> t
  val del_lit : lit -> t -> t
  val empty : unit -> t
  val from_string : string -> t
  val pp_clause : t -> string
end

module Clause : CLAUSE

type t
type stats = { num_vars : int; num_clauses : int }

val from_dimacs : string -> t
val to_dimacs : t -> (string, int) Hashtbl.t * string
val add_clause : Clause.t -> t -> t
val add_clauses : Clause.t list -> t -> t
val from_clauses : Clause.t list -> t
val stats : t -> stats
val empty : unit -> t
val pp_cnf : t -> string
