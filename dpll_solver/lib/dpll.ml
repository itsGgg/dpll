open List

(** Ce fichier contient les fonctions à compléter. *)

(** Fonctions utilitaires. *)

(** filter_map : ('a -> 'b option) -> 'a list -> 'b list
   disponible depuis la version 4.08.0 de OCaml dans le module List :
   pour chaque élément de `list', appliquer `filter' :
   - si le résultat est `Some e', ajouter `e' au résultat ;
   - si le résultat est `None', ne rien y ajouter.
   Attention, cette implémentation inverse l'ordre de la liste *)
let filter_map filter list =
  let rec aux list ret =
    match list with
    | []   -> ret
    | h::t -> match (filter h) with
      | None   -> aux t ret
      | Some e -> aux t (e::ret)
  in aux list []

(* Ceci est un exemple de test unitaire pour la fonction filter_map. *)
let%test "filter_map" =
  (filter_map (fun x -> if x mod 2 = 0 then Some (x * 2) else None) [1; 2; 3; 4]) = [8; 4]

(* Voici d'autres exemples de tests unitaires dont vous pouvez vous inspirer pour tester vos fonctions. *)
let rec big_list n acc =
  if n <= 0 then acc else big_list (n-1) (n::acc)

(* On teste d'abord sur une petite liste.*)
let %test "length vide" = (length [] = 0)
let %test "length 1" = (length [1] = 1)
let %test "length 3" = (length [1;2;3] = 3)
(* Puis sur une grande liste pour vérifier qu'il n'y a pas de débordement de pile. *)
let %test "length big" = let big = 10000000 in (length (big_list big []) = big)


(** Voici une proposition de types pour une implémentation simple.
    Vous pouvez éventuellement changer les types dans un deuxième temps. *)
type literal = int
type clause = literal list
type cnf = clause list
type interpretation = int list
type resultat = Sat of interpretation | Unsat

(** Avec ce choix de type cette fonction est l'identité. *)
let cnf_of_int_list_list (l: int list list) : cnf = l

(** print_modele : resultat -> unit afficher le résultat *)
let print_modele: resultat -> unit = function
  | Unsat   -> print_string "UNSAT\n"
  | Sat modele -> print_string "SAT\n";
     let modele_trie = sort (fun i j -> (abs i) - (abs j)) modele in
     List.iter (fun i -> print_int i; print_string " ") modele_trie;
     print_string "0\n"

(** simplifie : literal -> cnf -> cnf 
   applique la simplification de l'ensemble des clauses en mettant
   le littéral l à vrai *)
let simplifie l clauses =    
  (* simplifie_clause : simplifie une clause selon que le littéral l s'y trouve ou pas *)
  let simplifie_clause clause =
    (* Si la clause contient le littéral l, on peut la supprimer *)
    if List.mem l clause then
      None
    else 
      (* Sinon on reconstruit la clause sans le littéral -l *)
      Some (List.filter(fun y -> y <> -l) clause)
    (* On applique simplifie_clause à chaque clause avec filter_map, 
    puis on inverse la liste des clauses avec List.rev(), pour conserver l'ordre original des clauses*)
  in List.rev(filter_map (fun x -> simplifie_clause x) clauses)
  
(* CNF vide *)
let%test "simplifie_vide" = simplifie 1 [] = []
(* CNF avec clause vide *)
let%test "simplifie_clause_vide" = simplifie 1 [[]] = [[]]
(* CNF avec une clause contenant le littéral *)
let%test "simplifie_l" = simplifie 2 [[1;2;-2]] = []
(* CNF avec une clause ne contenant pas le littéral  *)
let%test "simplifie_l_2" = simplifie 1 [[-2;3]] = [[-2;3]]
(* CNF avec plusieurs clauses *)
let%test "simplifie_l_3" = simplifie 2 [[1;-2;2]; [-2;3]; [4]] = [[3];[4]]

(** solveur_split_rec : cnf -> interpretation -> resultat
   exemple d'utilisation de `simplifie' cette fonction ne doit pas être modifiée, sauf si vous changez 
   le type de la fonction simplifie *)
let rec solveur_split_rec clauses interpretation =
  (* l'ensemble vide de clauses est satisfiable *)
  if clauses = [] then Sat interpretation else
  (* la clause vide n'est jamais satisfiable *)
  if mem [] clauses then Unsat else
  (* branchement *) 
  let l = hd (hd clauses) in
  let branche = solveur_split_rec (simplifie l clauses) (l::interpretation) in
  match branche with
  | Unsat -> solveur_split_rec (simplifie (-l) clauses) ((-l)::interpretation)
  | _    -> branche

let solveur_split clauses = solveur_split_rec clauses []

(** Solveur dpll récursif *)

(** pur : int list list -> int (anciennement cnf -> literal option)
    - si `clauses' contient au moins un littéral pur, retourne
      ce littéral ;
    - sinon renvoie None *)
let pur clauses =

  (* on récupère tous les littéraux dans une liste *)
  let tous_litteraux = List.flatten clauses in

    (* on cherche le premier littéral qui n'a pas son opposé dans la liste *)
    match List.find_opt (fun x -> not (List.mem (-x) tous_litteraux)) tous_litteraux with

      (* on trouve un littéral pur, donc on le rentourne *)
      | Some l -> Some l

      (* on n'a trouvé aucun littéral pur, on renvoie None*)
      | None -> None

(* expression vide *)
let%test "pur_vide" = pur [] = None

(* aucun littéral pur *)
let%test "pur_aucun" = pur [[1; -2]; [2; -1]] = None

(* clauses vides acceptées *)
let%test "pur_clauses_vides" = pur [[]; []; [1]] = Some 1

(* premier littéral pur simple *)
let%test "pur_simple" = pur [[2; -3]; [3; 4]] = Some 2

(* premier littéral pur négatif *)
let%test "pur_negatif" = pur [[-3; 4]; [4; -5]; [-6]] = Some (-3)

(** unitaire : cnf -> literal option
    - si `clauses' contient au moins une clause unitaire, retourne
      le littéral de cette clause unitaire ;
    - sinon renvoie None *)
let rec unitaire clauses =
  match clauses with
  (* Si la liste des clauses est vide, pas de clause unitaire donc on renvoie None*)
  | [] -> None
  (* Sinon on vérifie la prèmiere clause *)
  | x::xs -> 
    (* Si la clause est de taille 1, on a alors une clause unitaire, donc on renvoie son littéral*)
       if List.length x = 1 
        then Some(List.hd(x))
    (* Sinon, on cherche dans le reste des clauses  *)
      else unitaire xs

(* CNF avec clause vide *)
let%test "unitaire_vide" = unitaire [[]] = None
(* CNF vide *)
let%test "unitaire_cnf_vide" = unitaire [] = None
(* CNF sans clause unitaire *)
let%test "unitaire_aucune" = unitaire [[1;2]; [-1;3]] = None
(* CNF avec clause unitaire *)
let%test "unitaire_simple" = unitaire [[2;3]; [1]] = Some 1
(* CNF avec plusieurs clauses unitaires *)
let%test "unitaire_multiple_units" =
  match unitaire [[3;4] ; [2] ; [1]] with
  | Some 1 | Some 2 -> true
  | _ -> false
 
(** solveur_dpll_rec : cnf -> interpretation -> resultat *)
let solveur_dpll_rec clauses interpretation =

  (* simplification deterministe: clauses unitaires, puis litteraux purs*)
  let rec simpl_deter cls interp =

    (* si cnf vide ou que cls a une clause vide *)
    if cls = [] || List.mem [] cls then (cls, interp)
    else match unitaire cls with 

      (* si une clause unitaire existe, on simplifie avec l, on l'ajoute à l'interpretation *)
      | Some l -> simpl_deter (simplifie l cls) (l::interp)
      | None -> (match pur cls with 

        (* si un litteral pur existe, on simplifie avec l, on l'ajoute à l'interpretation *)
        | Some l -> simpl_deter (simplifie l cls) (l::interp)
        | None -> (cls, interp))
  in let c, i = simpl_deter clauses interpretation in
  (* si apres simpl_deter la formule contient une clause vide, elle est Unsat *)
  if List.mem [] c then Unsat

  (* si la formule n'a plus de clauses, elle est Sat avec l'interpretation *)
  else if c = [] then Sat i
  else 
    
    let l = List.hd (List.hd c) in
      match solveur_split_rec (simplifie l c) (l::interpretation) with
        | Unsat -> solveur_split_rec (simplifie (-l) c) ((-l)::i)
        | sat -> sat


let solveur_dpll clauses = solveur_dpll_rec clauses []
