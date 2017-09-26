(* Student information:

   Enter your name, and if you chose to work in pairs, the name of the
   student you worked with (both students must submit the solution to
   mycourses):

   Name:	Spiros Mavroidakos
   Mcgill id:	260689391
*)

(* Homework 1 - questions 2 and 3 *)

(* First, some utility functions and declarations that you can use. be
   sure to check ocaml's documentation to find more functions
   available to you.

   You can start checking the documentation at:
   https://caml.inria.fr/pub/docs/manual-ocaml/libref/pervasives.html
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

(* Question 2: triangles are the best *)

type side = float

type tr_by_sides = side * side * side

type tr_kind
  = Scalene
  | Equilateral
  | Isosceles

(* Question 2.1 
	The logic is simple, if one of the possible true values fails,
	then false is returned. if they all pass then it is true as 
	explained in the qestion.
*)
let well_formed_by_sides (a, b, c : tr_by_sides) : bool =
	if ((a +. b) > c) then
  		if ((a +. c) > b) then
				if ((b +. c) > a) then true
					else false
		else false		
	else false


(* Question 2.2 
	creates a triangle based on the area and type of triangle.
	cases:
		1.equilateral:
			basic formula to get the dimensions. commonly known.
		2. isosceles:
			ab is the length of the sides a and b since they are equal.
			since an isosceles can be modeled as half of a square, ab 
			can be viewed as the length of a side of the square. knowing 
			that the area is the lenght of a side^2 for a square, you can
			multiply the area of the triangle by 2 and then sqrt it to get 
			the length. using pythagorean theorem, you can then find the length 
			of the 3rd side c.
		3. scalene:
			asumptions: the base is always equal to the area and the height is
			therefore always 2. 
			using these assumptions, if we partition the base so that it is
			split anywhere that is not the middle, then the resulting triangle 
			will be scalene. here i choose to partition it at 1/3 of the length 
			of the base. using those assumptions, you can use pythagorean theorem
			since you know 2 sides so you can find the 3rd.

*)
let create_triangle (kind : tr_kind) (area : float) : tr_by_sides = match kind with 
  	| Equilateral -> let (sides:side) = sqrt((4.0 *. area) /. sqrt(3.0)) in 
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

let sides_to_angle (a, b, c : tr_by_sides) : tr_by_angle option =
	match well_formed_by_sides (a,b,c) with
	|false -> None
	|true -> Some (a, b, acos((c *. c -. a *. a -. b *. b) /. (-2.0 *. a *. b)));;

let angle_to_sides (a, b, gamma) : tr_by_sides option =
	match well_formed_by_angle(a, b, gamma) with
	| false -> None
	| true -> Some (a, b, sqrt(a *. a +. b *. b -. 2.0 *. a *. b *. cos(gamma)));;
  

(* Now that you implemented Q2.2 and saw the new representation of
   triangles by two sides and the angle between them, also ponder on
   how one represents Q2.2 using this representation. The idea is to
   think about how the representation helps make illegal states hard
   to represent and how easy and hard it is to implement the
   algorithm. *)

(* Question 3: Flexing recursion and lists *)

let even (n : int) : bool = n mod 2 = 0

(* Question 3.1 
	Solution: Have a recursive function that takes in a list to be sorted and 2 
	empty lists. One of the empty list will contain all the evens and the other 
	will contain all the odds. If the value is even add it to the even list,
	otherwise add it to the odd list. When the end of the list is reached, the 
	lists are combined and returned.
*)
let evens_first (l : int list) : int list =
	let rec even_rec l acc_even acc_odd = match l with
	| [] -> acc_even@acc_odd
	| h::t -> if (even(h)) then even_rec t (acc_even@[h]) acc_odd
			else even_rec t acc_even (acc_odd@[h])
	
	in  
  	even_rec l [] [];;

let ex_1 = evens_first [7 ; 5 ; 10; 12; 6; 3; 4; 2; 1]
(* val ex_1 : int list = [2; 4; 6; 4; 2; 7; 5; 3; 1] *)

(* Question 3.2 
	Solution:Input is the list, an int for the longest streak and an int for the 
	current streak. Simply just increment the accumulators and reset the current 
	streak if an odd appears but compare the current to the longest to possibly 
	reassign the longest. Recursively call to go through the list.
*)
let even_streak (l : int list) : int =
	let rec streak l acc_old acc_new = match l with
	| [] -> if (acc_old > acc_new) then acc_old
		else acc_new
	| h::t -> if (even(h)) then streak t acc_old (acc_new + 1)
		else if (acc_old > acc_new) then streak t acc_old 0
			else streak t acc_new 0
 	in 
 	streak l 0 0

let ex_2 = even_streak [7; 2; 4; 6; 3; 4; 2; 1; 2; 2; 2; 2; 2]

(* val ex_2 : int = 3 *)

(* Question 3.3 *)

type nucleobase = A | G | C | T

let compress (l : nucleobase list) : (int * nucleobase) list =
	let rec dna_magic l counter current nucleo_l = match l with
  	|[] -> nucleo_l@[(counter, current)]
	|h::t -> if (counter = 0) then dna_magic t 1 h nucleo_l
		else if (current = h) then dna_magic t (counter + 1) current nucleo_l
			else dna_magic t 1 h (nucleo_l@[(counter,current)])
  	in
  	dna_magic l 0 A []

(*
	Helper Method for decompress. Take in a counter which is the 
	amount of times to print he nucleobase.
	nucleo is the nucleobase to be added to the list that will
	be returned.
	decompress_l is the list of nucleobase with the nucleobase in 
	it counter times.
*)
let rec decompress_printer counter nucleo decompress_l  = 
	if (counter > 0) then decompress_printer (counter - 1) nucleo (decompress_l@[nucleo])
	else decompress_l


let rec decompress (l : (int * nucleobase) list) : nucleobase list =
	let rec dna_helper l decomp_l = match l with
  	|[] -> decomp_l
  	|((s,k) as h)::t -> dna_helper t (decompress_printer s k decomp_l)
  	in
  	dna_helper l []

let sample_dna : nucleobase list = [A;A;A;A;G;G;A;T;T;T;C;T;C]

let ex_3 = compress sample_dna

let ex_4 = decompress ex_3

let res_3_4 = sample_dna = ex_4 (* This should be true if everything went well *)
