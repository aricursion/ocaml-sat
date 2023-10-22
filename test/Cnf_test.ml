open Cnf

let test1 () =
  let emp = Clause.empty () in
  let c1 = Clause.add_lit (true, "x1") emp in
  let c2 = Clause.add_lit (false, "x2") c1 in
  let c3 = Clause.add_lit (false, "x3") c2 in
  print_string (Clause.pp_clause c3);
  print_newline ();;

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
  let (_, dimacs_string) = Cnf.to_dimacs dimacs in 
  print_string dimacs_string;;

test1 ();;
print_newline();;
test2 ();;
print_newline ();;
test3 ();;