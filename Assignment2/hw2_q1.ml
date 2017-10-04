(* Name: Spiros Mavroidakos
 * Student Id: 260689391
 * Summary: Answers to Q1 of Assignment 2
 *
 * *)

type prop = Atom of string
          | Not of prop
          | And of prop * prop
          | Or of prop * prop

let impl (p, q) = Or(Not p, q)
let iff (p, q) = And (impl (p,q), impl (q, p))

let mp = impl (And (Atom "P", impl (Atom "P", Atom "Q")), Atom "Q")

(* Proof by contradiction (reduction ad absurdum): ((¬ P) ⇒ Q) ∧ ((¬ P) ⇒ (¬ Q)) ⇒ P *)
let ra = impl (
             And (impl (Not (Atom "P"),
                        Atom "Q"),
                  impl (Not (Atom "P"),
                        Not (Atom "Q"))),
             Atom "P")

(* Atoms and their negations *)
type signed_atom
  = PosAtom of string
  | NegAtom of string

(* In NNF negations can only be applied to atoms, this datatype only
   allows for propositions in NNF *)
type nnf
  = AndN of nnf * nnf
  | OrN of nnf * nnf
  | AtomN of signed_atom

(* Q1.2: Write the function nnf that converts propositions into NNF,
   notice that the typechecker will guide you in your answer as the
   type forces the result to be in NNF. Your job is to not forget any
   sub-parts of the proposition in the conversion.*) 
let rec to_nnf : prop -> nnf = function
  | Atom a -> AtomN (PosAtom a) 
  | Not (Atom a) -> AtomN (NegAtom a)
  | Not (Not p) -> to_nnf p
  | Not (And (p, q)) -> OrN ( to_nnf (Not p), to_nnf (Not q))
  | Not (Or (p, q)) -> AndN ( to_nnf (Not p), to_nnf (Not q))
  | And (p, q) -> AndN (to_nnf p, to_nnf q)
  | Or (p, q) -> OrN (to_nnf p, to_nnf q)
   

(* Q1.3: Write a datatype cnf that represents only propositions in
   cnf. Hint: You might want to use more than one type to be able to
   represent sub-expressions.*)
(*
 *Will represent the base case of just a signed atom and also the case of an or
 * *)
type cnf_sub
  = AtomC of signed_atom
  | OrC of cnf_sub * cnf_sub

(*
 * The Cnf_sub type handles the trivial case of an atom and the case of an or of atoms. The cnf type will contain both of those
 * since it contains the cnf_sub type in the SubCnf data type. The AndC will be able to and all expressions that are sub expressions 
 * coming from the cnf_sub so it will be able to represent all the cnf expressions.
 * This representation was chosen in order to reuse some of the code that existed in the Sat.ml file when using the other functions
 * that need to be implemented such as the distribute method but replacing the And with AndC and the or with OrC.
 * *)
type cnf 
  = SubCnf of cnf_sub
  | AndC of cnf * cnf 


(* Q1.4: Write the distribute and nnf_to_cnf functions using the new
   datatype. Hint: you may need more than one helper function. *)
let rec distribute : cnf * cnf -> cnf = function
  | p, AndC (q, r) -> AndC(distribute (p, q), distribute (p, r))
  | AndC(q, r), p ->  AndC(distribute (q, p), distribute (r, p))
  | SubCnf p, SubCnf q -> SubCnf (OrC (p, q))


let rec nnf_to_cnf : nnf -> cnf = function
  | AndN(p, q) -> AndC (nnf_to_cnf p, nnf_to_cnf q)
  | OrN(p, q) -> distribute (nnf_to_cnf p, nnf_to_cnf q)
  | AtomN a -> SubCnf (AtomC a)


let to_cnf (p :prop) : cnf = nnf_to_cnf (to_nnf p)


(* Q1.5: Write the new positives and negative atoms function as in the
   previous version 
   Basically just copy past from the past iteration with a new case being AndC
   *)
let rec positives = function
 | SubCnf AtomC (PosAtom a) -> [a]
 | SubCnf AtomC (NegAtom _) -> []  
 | SubCnf (OrC (p, q)) -> positives (SubCnf p) @ positives (SubCnf q)
 | AndC(p,q) -> positives (p) @ positives (q)
          
  
let rec negatives = function
 | SubCnf AtomC (PosAtom _) -> []
 | SubCnf AtomC (NegAtom a) -> [a]  
 | SubCnf (OrC (p, q)) -> negatives (SubCnf p) @ negatives (SubCnf q)
 | AndC(p,q) -> negatives (p) @ negatives (q)
 

(* Fill in the code for the intersection function from Q1.1 *)
let rec intersection l1 l2 = 
  let rec isIntersection element l2' = match l2' with
    | [] -> []
    | h::t -> if (element = h) then [element]
              else isIntersection element t
  in
  let rec getIntersectionList l1 l2 acc = match l1 with
    | [] -> acc
    | h::t -> getIntersectionList  t l2 (acc@(isIntersection h l2))
  in
  getIntersectionList l1 l2 [];;


(* Q1.6: Write the new cnf_tautology function *)
let rec cnf_tautology : cnf -> bool = function
 | AndC (p, q) -> cnf_tautology p && cnf_tautology q
 | p -> not ([] = intersection (positives p) (negatives p))


let taut (p : prop) : bool = cnf_tautology (to_cnf p)
let unsat (p : prop) : bool = taut (Not p)
let sat (p : prop) : bool = not (unsat p) 

let nc = Not (And (Atom "P", Not (Atom "P")))
let nc_taut = taut nc
