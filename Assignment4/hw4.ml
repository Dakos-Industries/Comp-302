(* Author: Spiros Mavroidakos
 * Student ID: 260689391
 * Assignment 4
 *)
(* Q1: A Rose by any Other Name Would Smell as Sweet *)

type 'a rose_tree = Node of 'a * ('a rose_tree) list

(* Find with exceptions *)

exception BackTrack

(* Q1.1 write a function that finds an element of the tree using backtracking with exceptions *)

let rec find_e (p : 'a -> bool) (t : 'a rose_tree) : 'a = match t with (* Function with exceptions *)
  | Node (x,[]) -> if (p x) then x else raise BackTrack
  | Node (x,h::t') -> if (p x) then x else (try (find_e p h) 
                                           with BackTrack -> find_e p (Node (x,t')))

(* Q1.1: write this function and it helper functions *)
let find (p : 'a -> bool)  (t : 'a rose_tree) : 'a option = (* call find_e and handle the exceptions *)
  try Some (find_e p t) with BackTrack -> None


(* Find with failure continuations *)

let rec find_k (p : 'a -> bool) (t : 'a rose_tree) (k : unit -> 'a option) : 'a option = match t with
  | Node (x,[]) -> if (p x) then (Some x) else k ()
  | Node (x,h::t') -> if (p x) then (Some x) else (find_k p h (fun () -> find_k p (Node (x,t')) k))


(* Q1.2: write this function and it helper functions *)
let find' (p : 'a -> bool)  (t : 'a rose_tree) : 'a option = (*  call find_k with the appropriate inital continuation *)
  find_k p t (fun () -> None)

(* Find all with continuations *)
(*
let rec find_all_k  (p : 'a -> bool) (t : 'a rose_tree) (k : 'a list -> 'b) : 'b =  
  let rec finder p t k accl = match t with
  | Node (x,[]) -> if (p x) then (x::accl) else k accl
  | Node (x,h::t') -> if (p x) then (finder p h k (x::accl)) else (finder p h (Node (x,t')) k accl)
  in
  finder p t k [] 


(* Q1.3: write this function and it helper functions *)
let find_all p t = find_all_k p t (fun l -> l)
*)
(* An example to use *)

let example = Node (7, [ Node (1, [])
                         ; Node (2, [Node (16, [])])
                         ; Node (4, [])
                         ; Node (9, [])
                         ; Node (11, [])
                         ; Node (15, [])
                         ])

let is_big x =  x > 10


(* Q2 : Rational Numbers Two Ways *)

type fraction = int * int

module type Arith =
  sig
    type t
    val epsilon : t             (* A suitable tiny value, like epsilon_float for floats *)

    val plus : t -> t -> t      (* Addition *)
    val minus : t -> t -> t     (* Substraction *)
    val prod : t -> t -> t      (* Multiplication *)
    val div : t -> t -> t       (* Division *)
    val abs : t -> t            (* Absolute value *)
    val lt : t -> t -> bool     (* < *)
    val le : t -> t -> bool     (* <= *)
    val gt : t -> t -> bool     (* > *)
    val ge : t -> t -> bool     (* >= *)
    val eq : t -> t -> bool     (* = *)
    val from_fraction : fraction -> t (* conversion from a fraction type *)
    val to_string : t -> string        (* generate a string *)
  end

module FloatArith : Arith =
struct
  type t = float
  let epsilon = epsilon_float
  let from_fraction (num, den) = float_of_int num /. float_of_int den

  let plus = (+.)
  let minus = (-.)
  let prod = ( *. )
  let div = ( /. )
  let abs = abs_float
  let lt = (<)
  let le = (<=)
  let gt = (>)
  let ge = (>=)
  let eq = (=)
  let to_string x = string_of_float x
end

(* Q2.1: Implement the Arith module using rational numbers (t = fraction) *)

module FractionArith : Arith = 
struct 
  type t = fraction;;
  let epsilon = ((1,1000000) : fraction);;
  let from_fraction (num,den) = ((num,den) : fraction);;
  
  (* A function that returns the Greatest Common Denominator of two numbers *)
  let rec gcd (u : int) (v : int) : int =
    if v <> 0 then (gcd v (u mod v))
    else (abs u)

  let plus ((x,y) : fraction) ((x2,y2): fraction) = let n1 = (x * y2) + (x2 * y) and n2 = y*y2 in 
                                                      let gcd = gcd n1 n2 in ( (n1/gcd,n2/gcd) :fraction);;
  let minus ((x,y) : fraction) ((x2,y2): fraction) = let n1 =(x * y2) - (x2 * y) and n2 =  y*y2 in
                                                       let gcd = gcd n1 n2 in ((n1/gcd,n2/gcd):fraction);;
  let prod ((x,y) : fraction) ((x2,y2): fraction) = let n1 = (x * x2) and n2 = ( y*y2) in 
                                                      let gcd = gcd n1 n2 in ((n1/gcd, n2/gcd):fraction);;
  let div ((x,y) : fraction) ((x2,y2): fraction) = let n1 = (x * y2) and n2 = (x2*y) in 
                                                     let gcd = gcd n1 n2 in ((n1/gcd, n2/gcd):fraction);;
  let abs ((x,y) : fraction) = ((abs x, abs y):fraction);;
  let lt ((x,y) : fraction) ((x2,y2): fraction) = (x * y2) < (x2 * y);;
  let le ((x,y) : fraction) ((x2,y2): fraction) = (x * y2) <= (x2 * y);;
  let gt ((x,y) : fraction) ((x2,y2): fraction) = (x * y2) > (x2 * y);;
  let ge ((x,y) : fraction) ((x2,y2): fraction) = (x * y2) >= (x2 * y);;
  let eq ((x,y) : fraction) ((x2,y2): fraction) = (x * y2) = (x2 * y);;
  let to_string ((x,y) : fraction) = string_of_int x ^ "/" ^ string_of_int y;;
end


module type NewtonSolver =
  sig
    type t

    val square_root : t -> t
  end

(* Q2.2: Implement a function that approximates the square root using  the Newton-Raphson method *)

module Newton (A : Arith) : (NewtonSolver with type t = A.t) = 
struct 
  type t = A.t;;      
  let rec findroot x acc = 
    let rec compute a x acc = 
      let next = A.div(A.plus (A.div a x) x) (A.from_fraction (2,1)) in
        if (A.le(A.abs(A.minus next x)) acc) then x else (compute a next acc)
    in
    compute x x acc;;
  let square_root t' = findroot t' A.epsilon;;
end 



(* Examples *)
(*
module FloatNewton = Newton (FloatArith) 
module RationalNewton = Newton (FractionArith) 

let sqrt2 = FloatNewton.square_root (FloatArith.from_fraction (5, 1)) ;;
let sqrt2_r = RationalNewton.square_root (FractionArith.from_fraction (5, 1));;

FloatArith.to_string sqrt2;;
FractionArith.to_string sqrt2_r;;
*)

(* Q3 : Real Real Numbers, for Real! *)

type 'a stream = { head : 'a  ; tail : unit -> 'a stream}

let rec nth z = function
  | 0 -> z.head
  | n -> nth (z.tail()) (n - 1)

let rec constant x = {head = x ; tail = fun () -> constant x }

(* Some examples *)

let sqrt2 =
  {head = 1 ; tail = fun () -> constant 2} (* 1,2,2,2,2... *)

let golden_ratio = constant 1   (* 1,1,1,1,1,1 *)

let rec take n z =
  if n = 1 then [z.head]
  else z.head::(take (n-1) (z.tail()))

  
(* Q3.1: implement the function q as explained in the pdf *)
let rec q z n = if n = 0 then 1 else (if n = 1 then (nth z 1)
                else ((nth z n) * (q z (n-1)) + (q z (n-2))))
;;


(* Q3.2: implement the function r as in the notes *)
let rec r z n = 
  let rec compute z n acc currentN priorQ = 
    if (n < currentN || nth z currentN = 0) then acc 
    else (if currentN = 0 then (compute z n (float_of_int z.head) (currentN + 1) (z.head))
          else ( let currq = (q z currentN) in compute z n (acc +. ((-1.0) ** float_of_int(currentN - 1)) /. 
                                                           (float_of_int(priorQ * currq))) (currentN + 1) currq))
  in
  compute z n 0.0 0 0
;;
    

(* Q3.3: implement the error function *)
let error z n = let currq = (q z n) in (1.0 /. float_of_int(currq *(currq + (q z (n-1)))))
;;

(* Q3.4: implement a function that computes a rational approximation of a real number *)
let rat_of_real z approx = 
  let rec compute_rat n = let current = (r z n) in (if (nth z n = 0) then (current) else(
                                                       if( (abs_float((r z (n+1)) -. current)) < approx) 
                                                       then (current) else (compute_rat (n+1))))
  in
  compute_rat 0
;;


let real_of_int n = { head = n ; tail = fun () -> constant 0}

(* Q3.5: implement a function that computes the real representation of a rational number   *)
let rec real_of_rat r = {head = int_of_float(floor r); tail = fun () -> if ((r -. float_of_int(int_of_float r)) < epsilon_float) then constant 0
                                                          else (real_of_rat (1.0 /. (r -. float_of_int(int_of_float r))))}

(* Examples *)

(* Approximations of the  irrational numbers we have *)

(* let sqrt_2_rat = rat_of_real sqrt2 1.e-5 *)
let golden_ratio_rat = rat_of_real golden_ratio 1.e-5 

(* To test the representation of rationals we can try this *)
let to_real_and_back n = rat_of_real (real_of_rat n) 0.0001 

(* e1 should be very close to 10 (it is exactly 10 in the model solution) *)
let e1 = to_real_and_back 10.0 

(* this is the float approximation of pi, not the real number pi *)
let not_pi = 2. *. acos 0. 

(* This should share the same 4 decimals with not_pi *)
let not_pi' = to_real_and_back not_pi 
