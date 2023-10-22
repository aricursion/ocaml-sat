module type CLAUSE = sig
  type lit = bool * string
  type t = lit list

  val add_lit : lit -> t -> t
  val del_lit : lit -> t -> t
  val empty : unit -> t

  (* Accepts strings of variable names with "-" to denote negation such as:
     x1 -x2 x3
     corresponding to
     x1 \/ -x2 \/ x3
  *)
  val from_string : string -> t

  (* Accepts a list of the form
     [(false, "x1"); (true, "x2"); (false, "x3")]
     corresponding to the clause -x1 \/ x2 \/ -x3
  *)

  val pp_clause : t -> string
end

module Clause : CLAUSE = struct
  type lit = bool * string
  type t = lit list

  let add_lit l c = l :: c
  let del_lit l c = List.filter (fun l' -> l' = l) c
  let empty () = []

  (* Accepts strings of variable names with "-" to denote negation such as:
     x1 x2 x3
     x1 -x2 x3
     -x1 -x2
  *)
  let from_string s =
    let helper lit_string acc =
      if lit_string.[0] = '-' then
        (false, String.sub lit_string 1 (String.length lit_string - 1)) :: acc
      else (true, lit_string) :: acc
    in
    let s = String.trim s |> String.split_on_char ' ' in
    List.fold_right helper s []

  (* Accepts a list of the form
     [(false, "x1"); (true, "x2"); (false, "x3")]
     corresponding to the clause
     -x1 \/ x2 \/ -x3
  *)

  let rec pp_clause c =
    match c with
    | [] -> ""
    | (b, s) :: cs ->
        let name = if b then s ^ " " else "-" ^ s ^ " " in
        name ^ pp_clause cs
end

module type CNF = sig
  type t

  val from_dimacs : string -> t
  val to_dimacs : t -> (string, int) Hashtbl.t * string
  val add_clause : Clause.t -> t -> t
  val add_clauses : Clause.t list -> t -> t
  val from_clauses : Clause.t list -> t
  val stats : t -> int * int
  val empty : unit -> t
  val pp_cnf : t -> string
end

module Cnf : CNF = struct
  module Set = Set.Make (String)

  type t = { clauses : Clause.t list; vars : Set.t; num_clauses : int }

  let from_dimacs filename =
    (* read_file copied from stack overflow *)
    let read_file filename =
      let lines = ref [] in
      let chan = open_in filename in
      try
        while true do
          lines := input_line chan :: !lines
        done;
        !lines
      with End_of_file ->
        close_in chan;
        List.rev !lines
    in
    let spec, lines =
      match read_file filename with
      | [] -> failwith "DIMACS file is wrong"
      | x :: xs -> (x, xs)
    in
    let parse_spec spec =
      let stat = String.split_on_char ' ' (String.trim spec) in
      match stat with
      | [ _; _; num_vars; num_clauses ] ->
          (int_of_string num_vars, int_of_string num_clauses)
      | _ -> failwith "DIMACS parsing fail"
    in
    let _, num_clauses = parse_spec spec in
    let lines =
      List.map (fun s -> String.sub s 0 (String.length s - 2)) lines
    in
    let clauses = List.map Clause.from_string lines in
    let hs = Set.empty in
    let flatten_clauses = List.flatten clauses in
    let hs = List.fold_right (fun (_, x) s -> Set.add x s) flatten_clauses hs in
    { clauses; vars = hs; num_clauses }

  let to_dimacs (cnf : t) =
    let ht_init var_set =
      let ht = Hashtbl.create (Set.cardinal cnf.vars) in
      let ctr = ref 0 in
      Set.fold
        (fun n () ->
          ctr := !ctr + 1;
          if not (Hashtbl.mem ht n) then Hashtbl.add ht n !ctr)
        var_set ();
      ht
    in
    let ht = ht_init cnf.vars in
    let rec disj_to_string (disj : Clause.t) =
      match disj with
      | [] -> ""
      | x :: xs -> (
          match x with
          | true, s ->
              string_of_int (Hashtbl.find ht s) ^ " " ^ disj_to_string xs
          | false, s ->
              "-" ^ string_of_int (Hashtbl.find ht s) ^ " " ^ disj_to_string xs)
    in
    let base =
      Printf.sprintf "p cnf %d %d\n" (Set.cardinal cnf.vars) cnf.num_clauses
    in
    let s =
      List.fold_left (fun s c -> s ^ disj_to_string c ^ "0\n") base cnf.clauses
    in
    (ht, s)

  let add_clause clause cnf =
    let s =
      List.fold_left
        (fun s (_, n) -> if not (Set.mem n s) then Set.add n s else s)
        cnf.vars clause
    in
    {
      clauses = clause :: cnf.clauses;
      vars = s;
      num_clauses = cnf.num_clauses + 1;
    }

  let add_clauses cls_lst cnf =
    List.fold_right (fun cls cnf_acc -> add_clause cls cnf_acc) cls_lst cnf

  let stats cnf = (Set.cardinal cnf.vars, cnf.num_clauses)
  let empty () = { clauses = []; vars = Set.empty; num_clauses = 0 }
  let from_clauses cls_list = add_clauses cls_list (empty ())

  let pp_cnf cnf =
    List.fold_left (fun s cls -> s ^ Clause.pp_clause cls ^ "\n") "" cnf.clauses
end
