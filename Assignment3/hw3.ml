(* 
 * Author: Spiros Mavroidakos
 * Student ID: 260689391
 * *)

(* Question 1 - Unfolding *)

(* This is the function unfold, take some time to compare it it fold.
   If fold_left and fold_right take lists to generate a value, this
   function takes a value that will generate a list. The relation
   between folds and unfolds is the beginning of a wonderful tale in
   computer science. But let's not get ahead of ourselves.

   Unfold takes a function that from a seed value it generates a new
   element of the list, and a the seed for the next element, another
   function to stop the generation, and an initial seed.
*)

let rec unfold (f: 'seed -> ('a * 'seed)) (stop : 'b -> bool) (b : 'seed) : 'a list =
  if stop b then []
  else let x, b' = f b in
       x :: (unfold f stop b')

let nats max = unfold (fun b -> b, b + 1) (fun x -> x > max) 0

(* Q1.1: Return the even numbers up-to max *)
let evens max = unfold (fun x -> if (x = 0) then (x,x + 2) else (x, x + 2)) (fun s -> s >= max) 0;;


(* Q1.2: Return the Fibonacci sequence up-to max *)
let binetPos = (1.0 +. sqrt(5.0)) /. 2.0;;
let binetNeg = (1.0 -. sqrt (5.0)) /. 2.0;;
let computeBinet x = int_of_float ((binetPos ** (float_of_int x) -. binetNeg ** (float_of_int x)) /. sqrt(5.0));;

let fib max = unfold (fun x -> computeBinet x , x + 1) (fun s -> computeBinet s >= max) 1;;

(* Q1.3: Return the list of rows of the Pascal triangle that are shorter than max *)
let rec fact x = 
  let rec compute y acc rollingTotal = 
    if (y = 0) then 1
    else
      if (acc <= y) then compute y (acc + 1) (acc * rollingTotal)
      else
        rollingTotal
  in
  compute x 1 1;;

let binThm n k = (fact n) / (fact(n - k) * fact k);;

let rec createRow x = 
  let rec row n l acc = 
    if (acc <= n) then row n (l@[binThm n acc]) (acc + 1)
    else
     l
  in
  row (x) [] 0;;

let pascal max = unfold (fun x -> createRow x, x + 1) (fun s -> s > max) 0;;

let rec zip (l1 : 'a list) (l2 : 'b list) :  ('a * 'b) list =
match l1, l2 with
| [], _ -> []
| _, [] -> []
| x::xs, y::ys -> (x, y):: zip xs ys


(* (Extra credit) Optional: implement zip with a single call to unfold *)


let zip' l1 l2 = unfold (fun (x::y, h::t) -> (x,h),(y,t)) (fun (x,y) -> match x, y with 
                                                                        | [], _ -> true 
                                                                        | _, [] -> true 
                                                                        | _, _ -> false) (l1,l2);; 

(* Question 2 *)

let ugly x =
  let rec ackermann m n = match (m , n) with
    | 0 , n -> n+1
    | m , 0 -> ackermann (m-1) 1
    | m , n -> ackermann (m-1) (ackermann m (n-1))
  in
  ackermann 3 x

let memo_zero (f : 'a -> 'b) : 'a -> 'b = f

(* Q2.1: Write a function that memoizes the last value called. *)
let memo_one (f : 'a -> 'b) : ('a -> 'b) = 
  let input = ref None in
  let output = ref None in
  (fun x -> if (Some x = !input) then match !output with 
                                        | Some y -> y
            else (input := Some x ; output := Some (f x); f x)
  )


(* Example usage:

let ugly' = memo_one ugly

let u1 = ugly' 3                (* this one calls ugly with 3 *)
let u2 = ugly' 3                (* this one uses the stored value *)
let u3 = ugly' 1                (* the stored value is for 3 so it calls ugly *)
let u4 = ugly' 2                (* the stored value is for 1 so it calls ugly *)
let u5 = ugly' 10               (* the stored value is for 2 so it calls ugly and takes a couple of seconds *)
let u6 = ugly' 10               (* the one uses the stored value and returns immediately *)
*)
 


(* Q2.2: Write a function that memoizes the last value called.  *)
let rec find arr element =
  let rec getIt arr element index =
    if (arr.(index) = element) then index
    else getIt arr element (index + 1)
  in
  getIt arr element 0
;;

let memo_many (n : int) (f : 'a -> 'b) : 'a -> 'b = 
  let input = Array.make n None in
  let output = Array.make n None in
  let index = ref 0 in
  (fun x -> if (List.exists(fun y -> (Some x) = y) (Array.to_list(input))) then match output.(find input (Some x)) with
                                                                           |Some y -> y
            else
              if (!index = n) then (index := 0 ; Array.set input !index (Some x) ; Array.set output !index (Some (f x)) ; index := 1 ; f x)
              else 
                      (Array.set input !index (Some x) ; Array.set output !index (Some (f x)) ; index := !index + 1; Array.to_list(output) ;f x)
  )
;;
(*
let ugly' = memo_many 3 ugly

let u1 = ugly' 3                (* this one calls ugly with 3 *)
let u2 = ugly' 3                (* this one uses the stored value *)
let u3 = ugly' 1                (* the stored value is for 3 so it calls ugly *)
let u4 = ugly' 2                (* the stored value is for 1 so it calls ugly *)
let u5 = ugly' 10               (* the stored value is for 2 so it calls ugly and takes a couple of seconds *)
let u6 = ugly' 10               (* the one uses the stored value and returns immediately *)
let u7 = ugly' 11
*)

(*
(* Question 3: Doubly-linked circular lists  *)

(* Circular doubly linked lists *)

(* The type of a cell (a non-empty circular list) *)
type 'a cell = { mutable p : 'a cell; data : 'a ; mutable n : 'a cell}

(* The type of possibly empty circular lists *)
type 'a circlist = 'a cell option

(* An empty circular list *)
let empty :'a circlist = None

(* A singleton list that contains a single element *)
let singl (x : 'a) : 'a circlist =
  let rec pointer = {p = pointer ; data = x ; n = pointer} in
  Some pointer

(* Rotate a list to next element *)
let next : 'a circlist -> 'a circlist = function
  | None -> None
  | Some cl -> Some (cl.n)

(* Rotate a list to previous element *)
let prev : 'a circlist -> 'a circlist = function
  | None -> None
  | Some cl -> Some (cl.p)

(* Q3.1: Write a function that add a new element at the beginning of a list *)
let cons (x : 'a)  (xs : 'a circlist) : 'a circlist = assert false

(* Q3.2: Write a function that computes the length of a list (Careful with the infinite loops)  *)
let rec length (l : 'a circlist) : int = assert false

(* Q3.3: Write a function that produces an immutable list from a circular list *)
let to_list (l : 'a circlist)  : 'a list = assert false

(* Once you've written cons you can use this function to quickly populate your lists *)
let rec from_list : 'a list -> 'a circlist = function
  | [] -> empty
  | x::xs -> cons x (from_list xs)

(* Q3.4: Write a function that reverses all the directions of the list *)
let rev (l : 'a circlist) : 'a circlist = assert false

(* (Extra credit) OPTIONAL: Write the map function as applied to lists *)
let map (f : 'a -> 'b) : 'a circlist -> ' b circlist = assert false

(* Some possibly useful functions (Wink, wink!) *)

(* A function that returns the Greatest Common Denominator of two numbers *)
let rec gcd (u : int) (v : int) : int =
  if v <> 0 then (gcd v (u mod v))
  else (abs u)

(* A function that returns the least common multiple of two numbers *)
let lcm (m : int) (n : int) : int  =
  match m, n with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / (gcd m n)


(* (Extra credit) OPTIONAL A function that compares two lists ignoring the rotation *)
let eq (l1 : 'a circlist) (l2 : 'a circlist) : bool = assert false

(* Some examples *)
(*
let ex = cons 12 (cons 43 (cons 34 (singl 3)))
let lex = to_list ex

let l1 = from_list [true; true ; false]
let l3 = from_list [true; true ; false ; true; true ; false]

let l4 = from_list ['a'; 'b'; 'a'; 'b']
let l5 = from_list ['a'; 'b'; 'a'; 'b'; 'a'; 'b']

let l6 = from_list ['a'; 'a']
let l7 = from_list ['a'; 'a'; 'a']

let l8 = from_list [1 ; 2 ; 3]
let l9 = from_list [3 ; 1 ; 2]  (* eq l8 l9 = true *)

let l10 = from_list [1 ; 2 ; 3]
let l11 = from_list [3 ; 2 ; 1]  (* eq l10 l11 = false *)
*) 
*)
