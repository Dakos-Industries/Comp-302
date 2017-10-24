open OUnit2;;

(*
 * Author: Spiros Mavroidakos
 * GitHub: Dakos-Industries
 * Summary: Test Cases for assignment 3
 * *)

let evens10 = [0;2;4;6;8;10];;
let evens15 = [0;2;4;6;8;10;12;14];;



let test1 test_ctxt = assert_equal evens10 (Hw3.evens 10);;
let test2 tes_ctxt = assert_equal evens15 (Hw3.evens 15);;

let suite = "Suite">:::
["test1">:: test1;
 "test2">:: test2;
 ];;

let ()=
run_test_tt_main suite;;
