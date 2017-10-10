(* Part 1: define a function that computes the parity of a list of
   booleans.

   What we want is a function even_parity such as even_parity l
   returns a boolean that combined with the elements of l the 
   number of 1’s (true) is even.
   This is a simple but important algorithm in data communication, its
   purpose is to detect errors (one bit flips). It is very easy to
   implement in circuit but it is not very reliable.
 *)

let rec even_parity = function
  | [] -> false
  | true::xs -> not (even_parity xs)
  | false::xs -> even_parity xs

(* This version is very natural but it is not tail recursive. Let's
   make the tail recursive version. *)

let even_parity_tr l =
  let rec parity p = function
    | [] -> p
    | p'::xs -> parity (p<>p') xs
  in
  parity false l

(* Part 2: Now prove that both functions are equivalent. You will need to use facts about the <> (XOR) operation *)

let ex_1 = [true]
let ex_2 = [true; true]
let ex_3 = [false; true; true; false; true]

let t_1 = even_parity ex_1 = even_parity_tr ex_1
let t_2 = even_parity ex_2 = even_parity_tr ex_2
let t_3 = even_parity ex_3 = even_parity_tr ex_3

(* High order - croupier for simplified roulette *)

(* We have a simplified roulette, where we have only two colours that
   we can bet but if zero comes out, everyone loses *)

type colour = Red | Black        (* The two colours we can bet on *)

type result = colour option      (* The result of a run, it could be one of the colours or no colour if zero came up *)

type bet = int * colour          (* The bet amount and to what colour *)


(* It is simple to see who won *)
let compute (am, col : bet) : result -> int = function
  | None -> 0
  | Some col' -> if col = col' then am * 2 else 0

(*
Solve all these questions without using recursion or pattern
matching on lists, but instead just use the HO functions we saw
in class.
 *)

let rec map f = function
 | []  -> []
 | h::t -> (f h)::(map f t)

let bet_list : bet list = [(100,Red);(2,Black);(20,Black);(700,Red);(300,Red);(50,Red);]

let bet_list' : bet list = [(100,Red);(300,Red);]

(* Q1:  given a list of bets compute the results *)
let bet_results l res = map (fun bet' -> compute bet' res) l  


let rec filter (p : 'a -> bool) l = match l with
  | []   -> []
  | x::l ->
     if p x then x::filter p l
     else filter p l
(* Q2: given a list of bets and a result compute a list of winning bets *)
let winners l res = filter (fun bet' -> (compute bet' res) > 0) l

(* Q3: given a list of bets and a result compute how much money the casino needs to pay back *)
let rec fold_right  f l b = match l with
  | []   -> b
  | h::t -> f h (fold_right f t b)

let pay_up l res = 
        let win = winners l res 
        in
        fold_right (fun (x,y : bet) b -> (compute (x,y) res) + b) win 0

(* Q4: given a list of bets and a result compute if everyone won *)
let allwin l res = List.for_all ( fun (x,y:bet) -> (compute(x,y) res) > 0) l

(* Q5: given a list of bets and a result compute if someone won *)

let some_win l res = List.exists (fun bet -> (compute bet res) > 0) l

(* Q6: given a list of bets return the highest winning *)

let highest l = 
        List.fold_right (fun (x,y:bet) (a,b:bet) -> if (x > a) then (x,y) else (a,b)) l (0,Red)


(* Level-up (a bit more complicated) *)

(* Q7: given a list of bets and a result compute the balance for the casino, how much it made *)
let test l res =
        List.filter (fun (x,y:bet) -> (compute (x,y) res) = 0) l

let casino_money l res = 
        let k = List.filter (fun (x,y:bet) -> (compute (x,y) res) = 0) l
        in
        List.fold_right (fun (x,y:bet) b -> x + b) k 0

(* Ninja level  *)
(* Q8: Can you sort the results by the amount they made? *)

let sorter l = List.sort (fun (x,y:bet) (a,b :bet) -> compare a x) l
