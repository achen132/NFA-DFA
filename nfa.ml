open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
      List.fold_right (fun x acc -> match x with (a, b, c) -> 
      if elem a (List.sort_uniq Stdlib.compare qs) && b = s then 
        if elem c (List.sort_uniq Stdlib.compare acc) then acc else 
        c::acc else acc) nfa.delta [];;

let rec e_help (nfa: ('q,'s) nfa_t) (qs: 'q list) (a: 'q list) = 
  List.fold_right (fun x acc -> if elem x (List.sort_uniq Stdlib.compare nfa.qs) 
    then if elem x (List.sort_uniq Stdlib.compare acc)
      then acc
      else e_help nfa (move nfa [x] None) (x::acc)
    else acc) qs a;;

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  e_help nfa qs [];;

let rec accept_help (nfa: ('q,char) nfa_t) (char: char list) (curr: 'q) (acc2: 'q list) = 
  match char with 
  h::t -> 
    let result = (List.fold_right (fun x acc -> 
    acc || (accept_help nfa t x [])
    ) 
    (move nfa [curr] (Some h)) false ) in
      if result = false then
      if elem curr (List.sort_uniq Stdlib.compare acc2) then false else
      (List.fold_right (fun x acc -> 
       if x = curr then
        acc else
        acc || (accept_help nfa char x (x::acc2))
        ) 
        (e_closure nfa [curr]) false)
      else result
  | _ -> List.fold_right (fun x acc -> 
  acc || (elem x (List.sort_uniq Stdlib.compare nfa.fs))) (e_closure nfa [curr]) false;;

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  accept_help nfa (explode s) nfa.q0 [];;

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

(*let new_help (nfa: ('q,'s) nfa_t) (q: 'q) = *)
(*List.fold_right (fun x acc -> (e_closure nfa (move nfa [q] (Some x)))::acc) nfa.sigma [];;*)
  (*List.fold_right (fun x acc ->) nfa.sigma [];;*)

let new_help (nfa: ('q,'s) nfa_t) (qs: 'q list) (x: 's) = 
  List.fold_right (fun q acc -> 
    match qs with [] -> []@acc
    |_ -> (e_closure nfa (move nfa [q] (Some x)))@acc) qs [];;

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  List.fold_right (fun x acc -> (new_help nfa qs x)::acc) nfa.sigma[];;


 (* List.fold_right (fun x acc -> (new_help nfa x)@acc) qs [];;*)


 (* List.fold_right (fun x acc -> match x with (a, b, c) -> *)
   (*   if elem a (List.sort_uniq Stdlib.compare qs)  *)
     (*   then if elem c (List.sort_uniq Stdlib.compare nfa.qs) *)
       (*   then acc@[(e_closure nfa [c])] *)
         (* else acc *)
       (* else acc@[]) nfa.delta [[]];; *)


let trans_help (nfa: ('q,'s) nfa_t) (qs: 'q list) (x: 's) = 
  (qs, Some x, new_help nfa qs x);;

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  List.fold_right (fun x acc -> (trans_help nfa qs x)::acc) nfa.sigma [];;

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if (List.fold_right (fun x acc -> elem x nfa.fs || acc) qs false)
    then [qs]
    else [];;




let final_help (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t) (h: 'q list) = 
  if new_finals nfa h = []
    then dfa.fs
    else union (new_finals nfa h) dfa.fs;;

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t = failwith "unimplemened";;

let rec nfa_to_dfa_step_mod (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) (worked: 'q list list) : ('q list, 's) nfa_t =
  match work with h::t -> 
        let newdfa = {dfa with 
          qs = union (union [h] dfa.qs) (new_states nfa h)
          ; delta = union (new_trans nfa h) dfa.delta
          ; fs = final_help nfa dfa h
        } in 
        let newlst = diff (union (new_states nfa h) t) worked in 
        nfa_to_dfa_step_mod nfa newdfa newlst (union [h] worked)
  | _ -> dfa;;

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let output = {
    qs = []
    ; sigma = nfa.sigma
    ; delta = []
    ; q0 = (e_closure nfa [nfa.q0])
    ; fs = []
  } in nfa_to_dfa_step_mod nfa output [output.q0] [];;
