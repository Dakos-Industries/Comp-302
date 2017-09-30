open OUnit2;;

(*
 * Author: Spiros Mavroidakos
 * GitHub: Dakos-Industries
 * Summary: Test Cases for assignment 2
 *
 *
 * *)
let taut1 = Sat.Not (Sat.And (Sat.Atom "P", Sat.Not (Sat.Atom "P")))

let taut2 = Sat.impl (Sat.And(Sat.Atom "P", Sat.Not (Sat.Atom "P")), Sat.Atom "Q")

let notaut = Sat.Not (Sat.And (Sat.Atom "P", Sat.Not (Sat.Atom "Q")))

let test1 test_ctxt = assert_equal true (Sat.taut taut1);;

let test2 test_ctxt = assert_equal false (Sat.taut notaut);;

let test3 test_ctxt = assert_equal true (Sat.taut taut2);;

let suite = "Suite Q1.1">:::
["test1">:: test1;
 "test2">:: test2;
 "test3">:: test3;];;

let ()=
run_test_tt_main suite;;
