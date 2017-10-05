open OUnit2;;

(*
 * Author: Spiros Mavroidakos
 * GitHub: Dakos-Industries
 * Summary: Test Cases for assignment 2
 *
 *
 * *)

(* 
 * Test Cases for Sat.ml
 *)
let taut1 = Sat.Not (Sat.And (Sat.Atom "P", Sat.Not (Sat.Atom "P")))

let taut2 = Sat.impl (Sat.And(Sat.Atom "P", Sat.Not (Sat.Atom "P")), Sat.Atom "Q")

let notaut = Sat.Not (Sat.And (Sat.Atom "P", Sat.Not (Sat.Atom "Q")))

let test1 test_ctxt = assert_equal true (Sat.taut taut1);;

let test2 test_ctxt = assert_equal false (Sat.taut notaut);;

let test3 test_ctxt = assert_equal true (Sat.taut taut2);;

(*
 * Test Cases For hw2_q1.ml Q1.2
 *)
let taut1' = Hw2_q1.Not (Hw2_q1.And (Hw2_q1.Atom "P", Hw2_q1.Not (Hw2_q1.Atom "P")))

let sample = Hw2_q1.OrN(Hw2_q1.AtomN (Hw2_q1.NegAtom "P"), Hw2_q1.AtomN (Hw2_q1.PosAtom "P"))

let simpleAtom = Hw2_q1.AtomN(Hw2_q1.PosAtom "A")

let simpleAtomC = Hw2_q1.SubCnf (Hw2_q1.AtomC(Hw2_q1.PosAtom "A"))

let test4 test_ctxt = assert_equal true (sample = Hw2_q1.to_nnf(taut1'));;

let test5 test_ctxt = assert_equal true (simpleAtom = Hw2_q1.to_nnf(Hw2_q1.Atom "A"))

let test6 test_ctxt = assert_equal true (Hw2_q1.cnf_tautology(Hw2_q1.to_cnf(taut1')))

let test7 test_ctxt = assert_equal true (simpleAtomC = Hw2_q1.to_cnf(Hw2_q1.Atom "A"))

let suite = "Suite">:::
["test1">:: test1;
 "test2">:: test2;
 "test3">:: test3;
 "test4">:: test4;
 "test5">:: test5;
 "test6">:: test6;
 "test7">:: test7;];;

let ()=
run_test_tt_main suite;;
