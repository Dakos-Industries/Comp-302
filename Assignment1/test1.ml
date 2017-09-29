open OUnit2;;

let test1 test_ctxt = assert_equal true (Hw1.well_formed_by_sides(2.,1.,2.));;

let sample : Hw1.nucleobase list = [A;A;A;G;G;C;T];;

let test2 test_ctxt = assert_equal sample (Hw1.decompress(Hw1.compress (sample)));;

let suite = "suits">:::
["test1">:: test1;
"test2">:: test2;];;

let ()=
run_test_tt_main suite;;
