open Cnf

let test1 () =
  let emp = Clause.empty () in
  let c1 = Clause.add_lit (true, "x1") emp in
  let c2 = Clause.add_lit (false, "x2") c1 in
  let c3 = Clause.add_lit (false, "x3") c2 in
  print_string (Clause.pp_clause c3);
  print_newline ()

let test2 () =
  let current_dir = Sys.getcwd () in
  let filename = Filename.concat current_dir "../../../test/dimacs/test1.cnf" in
  let dimacs = Cnf.from_dimacs filename in
  let dimacs_string = Cnf.pp_cnf dimacs in
  print_string dimacs_string

let test3 () =
  let current_dir = Sys.getcwd () in
  let filename = Filename.concat current_dir "../../../test/dimacs/test1.cnf" in
  let dimacs = Cnf.from_dimacs filename in
  let _, dimacs_string = Cnf.to_dimacs dimacs in
  print_string dimacs_string

let print_ht print_key print_val ht =
  Hashtbl.iter
    (fun a b -> Printf.printf "%s : %s\n" (print_key a) (print_val b))
    ht

let test4 () =
  let clause1 =
    Clause.empty ()
    |> Clause.add_lit (true, "x1")
    |> Clause.add_lit (false, "x2")
  in
  let clause2 =
    Clause.empty ()
    |> Clause.add_lit (true, "x2")
    |> Clause.add_lit (false, "x3")
  in
  let clause3 =
    Clause.empty ()
    |> Clause.add_lit (false, "x4")
    |> Clause.add_lit (false, "x1")
    |> Clause.del_lit (false, "x4")
  in
  let clause4 = [ (false, "x8"); (true, "x6") ] in
  let cnf = Cnf.from_clauses [ clause1; clause2; clause3; clause4 ] in
  let ht, s = Cnf.to_dimacs cnf in
  print_string s;
  print_newline ();
  print_ht Fun.id string_of_int ht
;;

test1 ();;
print_newline ();;
test2 ();;
print_newline ();;
test3 ();;
print_newline ();;
test4 ()
