(* Student information:

   Enter your name, and if you chose to work in pairs, the name of the
   student you worked with (both students MUST submit the solution to
   myCourses):

   Name:	Spiros Mavroidakos
   McGill ID:	260689391
*)

(* Homework 1 - Questions 2 and 3 *)

(* First, some utility functions and declarations that you can use. Be
   sure to check Ocaml's documentation to find more functions
   available to you.

   You can start checking the documentation at:
   https://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html
 *)

(* the value of pi *)
let pi : float =  acos ~-.1.0

(* a function to compare floats that allows for some imprecision *)
let cmp n m = abs_float (n -. m) < 0.0001

(* a simple test of positivity *)
let positive a = a > 0.0

(* a function to check if a is multiple of b *)
let is_multiple_of (a : float) (b : float) : bool =
  let m = a /. b in
  cmp (m -. floor m) 0.0

(* a function to check if a is between plus/minus b *)
let abs_btwn a b = a < b && a > ~-.b

(* Question 2: Triangles are the best *)

type side = float

type tr_by_sides = side * side * side

type tr_kind
  = Scalene
  | Equilateral
  | Isosceles

(* Question 2.1 *)
let well_formed_by_sides (a, b, c : tr_by_sides) : bool =
	if ((a +. b) > c) then
  		if ((a +. c) > b) then
				if ((b +. c) > a) then true
					else false
		else false		
	else false


(* Question 2.2 *)
let create_triangle (kind : tr_kind) (area : float) : tr_by_sides = match kind with 
  	| Equilateral -> let (sides:side) = sqrt((4.0 *.area) /. sqrt(3.0)) in 
  			(sides,sides,sides) 
  	| Isosceles -> let ab:float = sqrt(2.0 *. area) in
  			let c:side = sqrt(2.0 *. ab *. ab) in
			(ab, ab, c)
  	| Scalene ->  let c = area in 
			let b = sqrt(4.0 +. (c *. c /.9.0)) and
			    a = sqrt(4.0 +. (4.0 *. c *. c /. 9.0)) in
			(a,b,c)

(* Question 2.3 *)
type angle = float

type tr_by_angle = side * side * angle

let well_formed_by_angle (a, b, gamma) : bool =
  (positive a && positive b && positive gamma) &&
    (not (is_multiple_of gamma pi))

let sides_to_angle (a, b, c : tr_by_sides) : tr_by_angle =
	let gamma = acos((c *. c -. a *. a -. b *. b) /. (a *. b)) in
	(a, b, gamma)

let angle_to_sides (a, b, gamma) : tr_by_sides =
	let c = a *. a +. b *. b -. 2.0 *. a *. b *. cos(gamma) in
	(a, b, c)
  

(* Now that you implemented Q2.2 and saw the new representation of
   triangles by two sides and the angle between them, also ponder on
   how one represents Q2.2 using this representation. The idea is to
   think about how the representation helps make illegal states hard
   to represent and how easy and hard it is to implement the
   algorithm. *)

(* Question 3: Flexing recursion and lists *)

let even (n : int) : bool = n mod 2 = 0

(* Question 3.1 *)
let evens_first (l : int list) : int list =
 let rec even_rec l acc_even acc_odd = match l with
	| [] -> acc_even@acc_odd
	| h::t -> if (even(h)) then even_rec t (h::acc_even) acc_odd
			else even_rec t acc_even (h::acc_odd)
	
	in  
  even_rec l [] []
;;		 
let ex_1 = evens_first [7 ; 5 ; 2; 4; 6; 3; 4; 2; 1]
(* val ex_1 : int list = [2; 4; 6; 4; 2; 7; 5; 3; 1] *)

(* Question 3.2 *)
let even_streak (l : int list) : int =
  assert false

let ex_2 = even_streak [7; 2; 4; 6; 3; 4; 2; 1]

(* val ex_2 : int = 3 *)


(* Question 3.3 *)

type nucleobase = A | G | C | T

let compress (l : nucleobase list) : (int * nucleobase) list =
  assert false

let rec decompress (l : (int * nucleobase) list) : nucleobase list =
  assert false

let sample_dna : nucleobase list = [A;A;A;A;G;G;A;T;T;T;C;T;C]

let ex_3 = compress sample_dna

let ex_4 = decompress ex_3

let res_3_4 = sample_dna = ex_4 (* This should be true if everything went well *)
