(*================================================================================
  ============================ Structure des données =============================
  ================================================================================*)
type ghost = U of int | R | B (* Unknown, Red, Blue *)
type placement = int -> ghost
type player = Known | Unknown (* Généralement : Known = Bot, Unknown = Utilisateur *)

type state = player * string * int (* player : le joueur dont c'est le tour
                                      string : le plateau, 
                                      int : le nombre de fantômes bleus inconnus morts. 
                                            -1 si un fantôme bleu est sorti*)

type pending_state = int * (ghost->state) (* int : le fantôme inconnu qui a besoin d'être revelé, 
                                          0 si aucun fantôme n'a besoin d'être révelé
                                            ghost -> state : fonction qui renvoie un état en fonction de la couleur
                                              du fantôme qui a besoin d'être révélé. *)


(*================================================================================
  ================================= Paramètres ===================================
  ================================================================================*)
let configuration_6x6 = (6, 6, 4, 4, [25;26;27;28;31;32;33;34], [1;2;3;4;7;8;9;10], [0; 5], [30;35],  [6; 1; -6; -1])
let configuration_4x4 = (4, 4, 1, 1, [13;14], [1;2], [0;3], [12;15], [4; 1; -4; -1])




let (board_width, board_height, 
  number_of_blue_ghosts, number_of_red_ghosts, 
  known_placement_cases, unknown_placement_cases, 
  known_exit_cases, unknown_exit_cases, 
  possible_moves) = configuration_4x4
let board_length = board_width * board_height
let number_of_ghosts = number_of_blue_ghosts + number_of_red_ghosts
let search_time = 1.0

let () = assert (number_of_blue_ghosts + number_of_red_ghosts = List.length known_placement_cases)
let () = assert (number_of_blue_ghosts + number_of_red_ghosts = List.length unknown_placement_cases)

(*================================================================================
  ============================ Fonctions utilitaires =============================
  ================================================================================*)
let player (p, _, _ : state) : player = 
  (* post-condition : renvoie le joueur donc c'est le tour *)
  p

let other = function Known -> Unknown | Unknown -> Known

let board (_, b, _ : state) : string = 
  (* post-condition : renvoie la chaine de caractères décrivant le plateau *)
  b

let dead_blue_ghosts (_, _, d) : int = 
  (* post-condition : renvoie le nombre de fantômes bleus inconnus morts *)
  d


let char (g : ghost) : char =
  match g with
  | U(_)-> '?' 
  | R -> 'R' 
  | B -> 'B' 

let string (p : player) : string = 
  match p with
  | Known -> "Known"
  | Unknown -> "Unknown"


let print_state ((p, b, d) : state) : unit = 
  for i = (board_height-1) downto 0 do
    Printf.printf "|";
    for j = 0 to (board_width-1) do
      Printf.printf " %c " b.[i*board_width+j]
    done;
    Printf.printf "|\n"
  done;
  Printf.printf "Tour : %s\n" (string p);
  if d > -1 then
    Printf.printf "Nombre de fantômes bleus inconnus morts : %d\n" d
  else 
    Printf.printf "Un fantôme bleu inconnu est sorti !\n"

let print_state_for_unknown (p, b, d : state) : unit = 
  let blue_cnt = ref 0 and red_cnt = ref 0 and exited = ref false in
  for i = (board_height-1) downto 0 do
    Printf.printf "%d |" (i+1);
    for j = 0 to (board_width-1) do
      let k = i*board_width+j in
      match b.[k] with 
      | 'B' -> blue_cnt := 1 + !blue_cnt; 
               if List.mem k unknown_exit_cases then exited := true; 
               Printf.printf " ? "
      | 'R' -> red_cnt  := 1 + !red_cnt ; 
               Printf.printf " ? "
      | '.' -> Printf.printf " . "
      | i -> Printf.printf " %c " i
    done;
    Printf.printf "|\n"
  done;
  Printf.printf "   "; for j = 0 to (board_width-1) do Printf.printf " %c " (char_of_int (97+j)) done; Printf.printf "\n";
  
  Printf.printf "Tour : %s\n" (string p);
  if !exited then Printf.printf "Un fantôme bleu inconnu est sorti coucou !\n";
  Printf.printf "Nombre de fantômes bleus inconnus morts : %d\n"  (number_of_blue_ghosts - !blue_cnt);
  Printf.printf "Nombre de fantômes rouges inconnus morts : %d\n" (number_of_red_ghosts  - !red_cnt )

let print_list (print_elt : 'a -> unit) (l : 'a list) : unit = 
  let rec aux = function 
            | [] -> print_string "]\n"
            | e :: l' -> Printf.printf ";";
                         print_elt e;
                         aux l' in
  Printf.printf "["; 
  match l with
  | [] -> Printf.printf "]\n"
  | e :: l' -> print_elt e; aux l'

let list_to_array (l : 'a list) (n : int) : 'a array =
  (* pré-condition : l est de taille n
     post-condition : tableau contenant les mêmes éléments que l dans le même ordre *)
  if l = [] then [||] else
  let a = Array.make n (List.hd l)  in
  let rec aux (l : 'a list) (i : int) : 'a array = 
    match l with
    | [] -> a
    | e :: l -> a.(i)<-e; aux l (i+1)
  in
  aux l 0

let print_placement (pl : placement) : unit = 
  Printf.printf "==========\n";
  for i = 0 to number_of_ghosts-1 do
    Printf.printf "%d -> %c\n" i (char (pl i))
  done;
  Printf.printf "==========\n"

let () = assert (list_to_array [1;2;3;4;5;6;7;9] 8 = [|1;2;3;4;5;6;7;9|])




(*===========================================================================================================
  ============================= Fonctions sur le jeu fantômes contre fantômes ===============================
  ===========================================================================================================*)

let count_unknown (p, b, d : state) : int = 
  let rec inner_count_unknown (k : int) : int =
    (* Compte le nombre de fantômes inconnus*)
    if k = -1 then 
      0 
    else
      (match b.[k] with | '.' | 'B' | 'R' -> 0 | _ -> 1) + inner_count_unknown (k-1) in
    inner_count_unknown (board_length -1)

let outcome ((p, b, d) as s : state) : player option = 
  (* coût temporel : C(outcome) = O(board_length) *)
  let rec count_known_blues (k : int) : int =
    (* Compte le nombre de fantômes bleus connus *)
    if k = -1 then 
      0
    else
      (match b.[k] with |'B' -> 1 | _ -> 0) + count_known_blues (k-1) in
  let rec count_known_reds (k : int) : int = 
    (* Compte le nombre de fantômes rouges connus *)
    if k = -1 then 
      0
    else
      (match b.[k] with |'R' -> 1 | _ -> 0) + count_known_reds (k-1) in
  let rec check_exited_ghost () : bool =
    (* Regarde si un fantôme bleu connu est sur une case de sortie *)
    List.exists (fun k -> b.[k] = 'B') known_exit_cases in
  
  (* On regarde d'abord si le joueur connu a gagné*)
  if check_exited_ghost () || d = number_of_blue_ghosts || count_known_reds (board_length-1) = 0 then
    Some(Known)
  (* Puis on regarde si le joueur inconnu a gagné*)
  else if d = -1 || count_known_blues (board_length-1) = 0 
                 || count_unknown s - (number_of_blue_ghosts - d) = 0 then
    Some(Unknown)
  else
    None

(* let () = assert (outcome (snd random_init) = None)
let () = assert (outcome (Known, ".R..........B01.", 0) = Some(Known))
let () = assert (outcome (Unknown, ".R..........B01.", 0) = Some(Known))
let () = assert (outcome (Known, "1...R......B...0", -1) = Some(Unknown))
let () = assert (outcome (Unknown, ".....RB..1......", 1) = Some(Known))
let () = assert (outcome (Unknown, "......B..10.....", 0) = Some(Known))
let () = assert (outcome (Known, ".....1R...0.....", 0) = Some(Unknown))
let () = assert (outcome (Known, "R...1......B....", 0) = Some(Unknown)) *)

let possible_move (k_from : int) (k_to : int) : bool = 
  (*post-condition : true si la case de départ est occupée par un fantôme du joueur donc c'est le tour 
      et si le déplacement figure parmi les mouvements possibles pour les fantômes 
      (à savoir déplacement de 1 à gauche, de 1 à droite, de 1 en haut et de 1 en bas), false sinon *)
  List.mem (k_to - k_from) possible_moves

let valid_move (p, b, d : state) (k_from : int) (k_to : int) : bool = 
  (* post-condition : true si le déplacement est valide sur le plateau (vérifie que la case d'arrivée est sur 
        le plateau et qu'elle ne contient pas de fantôme ami ), false sinon 
      !Attention! : Ne vérifie pas que le mouvement est parmi les mouvements possibles pour un fantôme
      
  coût temporel : C(valid_move) = O(1) *)
  if k_to < 0 || k_to >= board_length 
    || (k_from mod board_width = 0 && k_to - k_from = -1) 
    || (k_from mod board_width = (board_width-1) && k_to - k_from = 1) then
      false
  else 
    match p with
    | Known ->    begin match b.[k_to] with
                        | 'B' | 'R' -> false
                        | _ -> true
                  end
    | Unknown ->  begin match b.[k_to] with
                        | 'B' | 'R' | '.' -> true
                        | _ -> false
                  end

(* possible_move et valid_move : pourquoi faire deux fonctions différentes ?
possible_move : conditions nécessaires pour qu'un pour qu'un coup soit correcte mais déjà vérifiées 
  lors des appels depuis la fonction moves 
valid_move : conditions nécessaires pour qu'un coup soit correcte mais non déjà vérifiées lors 
  des appels depuis la fonction moves
Ainsi, depuis la fonction moves on n'appelle que valid_move, tandis que depuis la fonction user_move
on appelle les deux. *)



let create_move (p, b, d : state) (k_from : int) (k_to : int) : pending_state =
  (* coût temporel : C(create_move) = longueur(unknown_exit_cases) + C(int_of_char)
                                    ~ O(1)   *)
  (match p with
  | Known -> if b.[k_to] = '.' then -1 else (int_of_char b.[k_to])-48
  | Unknown -> if List.mem k_to unknown_exit_cases then (int_of_char b.[k_from])-48 else -1), 
  fun (g : ghost) -> 
    (* Coût temporel : C(fn) = O(board_length) *)
    let p' = other p in
    let s' = String.init board_length (fun k' -> if k'= k_from then '.' else if k' = k_to then b.[k_from] else b.[k']) in
    let d' = if g = B then match p with | Known -> d+1  (* Ici le fantôme révélé g est forcement un fantôme inconnu qu'on vient de manger. *)
                                        | Unknown -> -1 (* Ici le fantôme révélé g est forcement un fantôme inconnu qui vient d'arriver sur une case de sortie *)
             else d 
    in (p',s',d') 

let moves ((p, b, d) as s : state) : pending_state list = 
  (* coût temporel : C(moves) = C(outcomes) + C(known_moves/unknown_moves)
                              = O(board_length) *)
  let rec ghost_moves (k: int) (l : int list) (m : pending_state list): pending_state list =
    (* entrées : 
      k : la position du fantôme
      l : une liste des mouvements possibles 
      
    coût temporel : C(ghost_moves) = longueur(l) * (C(valid_move) + C(create_move))
                                   ~ O(longueur(l)) ~ O(1) *)
    match l with
    | [] -> m
    | e :: l -> if valid_move s k (k+e) then
                  ghost_moves k l (create_move s k (k+e) ::m)
                else
                  ghost_moves k l m 
    in
  let rec known_moves (k : int) (m : pending_state list) : pending_state list = 
    (* coût temporel : C(known_moves) = k * C(ghost_moves) 
                                      ~ O(k) *)
    if k = -1 then m
    else match b.[k] with
    | 'B' | 'R' -> known_moves (k-1) (ghost_moves k possible_moves m)
    | _ -> known_moves (k-1) m
    in
  let rec unknown_moves (k : int) (m : pending_state list) : pending_state list = 
    (* coût temporel : C(unknown_moves) = k * C(ghost_moves)
                                        ~ O(k) *)
    if k = -1 then m
    else match b.[k] with
    | 'B' | 'R' | '.' -> unknown_moves (k-1) m
    | _ -> unknown_moves (k-1) (ghost_moves k possible_moves m) in
    
  
  match outcome s with
  | Some _ -> []
  | None -> begin match p with
            | Known -> known_moves (board_length-1) []
            | Unknown -> unknown_moves (board_length-1) []
  end


let play (pl : placement) (g, fn : pending_state) : state = 
  (* coût temporel : C(play) = C(fn) + C(pl) = O(board_length) *)
  if g = -1 then fn (U (-1)) else fn (pl g) (* Si g = -1, i.e. aucun fantôme n'est demandé, 
                                          on donne U(-1) qui n'est pas un fantôme valide,  
                                          mais il ne sera pas utilisé donc ce n'est pas grave *)












(*===========================================================================================================
  ====================== Fonctions et algorithmes sur les dispositions des fantômes =========================
  ===========================================================================================================*)


(* Type auxiliaire permettant de créer des placements de manière récursive 
Généralement, le fantôme d'indice i dans la liste correspond au fantôme inconnu de numéro i.*)
type list_placement = ghost list 

let list_placement_to_placement (l_p : list_placement) : placement = 
  let a = list_to_array l_p number_of_ghosts in
    (fun i -> a.(i))


let () = Random.self_init () (* Initialisation du module Random, autrement il donne toujours les mêmes valeurs
                                au fil des éxécutions.*)
let random_placement_list () : list_placement = 
  let rec aux (blues : int) (reds : int) : list_placement = 
    match (blues, reds) with
    | 0, 0 -> []
    | 0, _ -> R::aux blues (reds-1)
    | _, 0 -> B::aux (blues-1) reds
    | _, _ -> if Random.bool () then
                R::aux blues (reds-1)
              else
                B::aux (blues-1) reds 
  in aux number_of_blue_ghosts number_of_red_ghosts

let random_placement () : placement = 
  list_placement_to_placement (random_placement_list ())

let create_initial_state (pl : placement): state = 
  let known_cnt = ref (-1) in
  let unknown_cnt = ref (-1) in
  let fill (k : int) : char = 
    if List.mem k known_placement_cases then (
      known_cnt := !known_cnt + 1;
      char (pl !known_cnt)
    ) else if List.mem k unknown_placement_cases then (
      unknown_cnt := !unknown_cnt + 1;
      char_of_int (48 + !unknown_cnt) 
    ) else '.'
  in (Known, String.init board_length fill, 0)

let random_initial_state () : state = 
  create_initial_state (random_placement ())


let possible_list_placements ((p, b, d) as s : state) : list_placement list =
   (* post-condition : Une liste contenant tous les dispositions possibles sous forme de listes des fantômes inconnus pour l'état s.
    Ces dispositions ne prennent pas en compte les fantômes éliminés, ils sont donc attribués à U dans ces dispositions.*)
  
  let rec aux (i : int) (unknown_ghosts : int list) (l_p : list_placement) (blues : int) (reds : int) (acc : list_placement list ): list_placement list = 
    (* préconditions : - -1 <= i < number_of_ghosts
                      - unknown_ghosts est une liste des numéros de fantômes inconnus encore en jeu
                        inférieurs ou égaux à i et triés par ordre décroissant *)
    if i = -1 then l_p::acc else
    match unknown_ghosts with 
    | [] -> aux (i-1) unknown_ghosts ((U i)::l_p) blues reds acc
    | i' :: unknown_ghosts' -> 
      if i = i' then (* Le fantôme inconnu i est encore en jeu *)
        if blues = number_of_blue_ghosts then
          aux (i-1) unknown_ghosts' (R::l_p) blues (reds+1) acc
        else if reds = number_of_red_ghosts then
          aux (i-1) unknown_ghosts' (B::l_p) (blues+1) reds acc
        else
          aux (i-1) unknown_ghosts' (R::l_p) blues (reds+1) 
            (aux (i-1) unknown_ghosts' (B::l_p) (blues+1) reds acc)
      else (* Le fantôme inconnu i est éliminé *)
        aux (i-1) unknown_ghosts ((U i)::l_p) blues reds acc in 
  
  let unknown_ghosts = List.sort (fun a b -> -compare a b) (String.fold_left (fun acc -> function
                                                                       | 'R' | 'B' | '.' -> acc 
                                                                       | c -> let i = (int_of_char c)-48 in i::acc) [] b) in
  aux (number_of_ghosts-1) unknown_ghosts [] d (number_of_ghosts - (count_unknown s) - d)  []
let possible_placements (s : state) : placement list = 
  (* post-condition : Une liste contenant tous les dispositions possibles des fantômes inconnus pour l'état s.
    Ces dispositions ne prennent pas en compte les fantômes éliminés, ils sont donc attribués à U dans ces dispositions.*)
  (* Printf.printf "Fantômes inconnus encore en jeu triés par ordre croissant : \n";
  print_list print_int unknown_ghosts;
  Printf.printf "Tous les placements possibles sous formes de liste : \n";
  print_list (print_list (fun g -> print_char (char g))) (create_list_placements 0 unknown_ghosts); *)
  List.map (fun l_p -> list_placement_to_placement l_p) (possible_list_placements s)

let all_possible_placements : placement list  = 
  possible_placements (random_initial_state ())

let all_initial_states : state list =
  List.map create_initial_state all_possible_placements

let all_initial_pending_states : pending_state list = 
  (* Tous les placements initiaux possibles pour le joueur Known sous forme de pending_state 
    pour pouvoir être assimilés à des moves.*)
  List.map (fun s -> (-1, fun g -> s)) all_initial_states






















(*==========================================================================
  ===================== Algorithmes avec heuristiques ======================
  ==========================================================================*)
let eval_perfect_state (pl : placement) (p, b, g : state) : int =
  (* coût temporel : C(eval_perfect_state) =  board_length * (C(distance) + C(int_of_char))
                                           =  O(board_length)  *)
  let distance (k : int) (exit_cases : int list) : int = 
    (* coût temporel : C(distance) = O(longueur(exit_cases)) 
                                   ~ O(1) *)
    let i, j  = k / board_width, k mod board_width in
    List.fold_left (fun min_d exit -> let exit_i, exit_j = exit / board_width, exit mod board_width in 
                                      let d = abs (exit_i - i) + abs (exit_j - j) in min min_d d) 
                    max_int exit_cases
  in
  let rec loop (k : int) (acc : int) : int =
    if k = -1 then acc else 
    let add = 
      match b.[k] with
      | '.' -> 0
      | 'R' -> -13
      | 'B' -> 10 - (distance k known_exit_cases)
      | c -> let i = int_of_char c - 48 in
        begin match pl i with
        | B -> -10 + (distance k unknown_exit_cases)
        | R -> 13
        | U _ -> failwith "A placement can't return an Unknown ghost."
        end
    in loop (k-1) (acc+add)
  in loop (board_length-1) 0


let rec alphabeta (pl : placement) ((p, _, _) as s : state) (d: int) (a : int) (b: int) : int = 
  (* minmax avec élagage alpha_beta. Le joueur max est Known.
  
  coût temporel : C(alphabeta) = C(moves s) + C(outcome s) + C(eval_perfect_state) + longueur(moves s) * (C(play) + C(alphabeta)) 
                               = O(longueur(moves s) * (O(board_length)+C(alphabeta) ))    *)
  match moves s with
  | [] -> begin match outcome s with
                | Some Known -> 100
                | Some Unknown -> -100
                | None -> 0
          end
  | _ when d = 0 -> eval_perfect_state pl s
  | l -> 
    let rec loop (v: int) (a : int) (b : int) = function
      | [] -> v
      | s' :: l -> 
        let e = alphabeta pl (play pl s') (d-1) a b in 
        match p with
        | Known -> (* joueur max *)
          let v = max v e in
          if v  >= b then v else loop v (max a v) b  l
        | Unknown -> (* joueur min *)
          let v = min v e in
          if v <= a then v else loop v a (min b v) l
    in
    loop (if p = Known then min_int else max_int) a b l
let alphabeta (pl : placement) (s : state) (depth): int = 
  (* appelle alphabeta à des profondeurs successives, c'est un parcours en profondeurs itérés
  let rec iterative_deepening_search (end_time : float) (depth : int) (acc : int): int = 
    if Sys.time () > end_time then (
      Printf.printf "Recherche effectuée à la profondeur %d. \n" (depth-1);
      acc
    )else (
      Printf.printf "Profondeur %d.\n" depth;
      Printf.printf "Sys.time : %f.\n" (Sys.time ());
      Printf.printf "End time : %f.\n" end_time;
      iterative_deepening_search end_time (depth+1) (alphabeta pl s depth (-100) (+100))
     ) in 
  iterative_deepening_search (Sys.time () +. search_time) 1 0 *)

  alphabeta pl s depth (-100) (+100)




let rec average (possible_placements : placement list) (p_s : pending_state) (depth : int) (sum : int) (count : int) : int =
  (* coût temporel : C(average) = longueur(possible_placements) * ( C(alphabeta) + C(play) )*)
  match possible_placements with
  | [] -> sum / count
  | pl :: l -> average l p_s depth (sum + alphabeta pl (play pl p_s) depth) (count+1) 
let average (possible_placements : placement list) (p_s : pending_state) (depth : int): int = 
  (*Printf.printf "Appel de la fonction average : \n - possible_placements : ";
  print_list print_placement possible_placements; *)
  average possible_placements p_s depth 0 0

let best_average (possible_placements : placement list) (moves : pending_state list) : pending_state * int = 
  (* entreés : 
    - bp_s : best pending state
    - bscore : best score 
  postconditions : meilleur mouvement avec le score associé. Ils sont calculés avec alphabeta par parcours en profondeurs itérés.*)
  let end_time = Sys.time () +. search_time in
  let rec iterative_deepening_search (depth : int) (bp_s : pending_state) (bscore : int) (l : pending_state list) : pending_state * int = 
    if Sys.time () > end_time then (Printf.printf "Profondeur du calcul : %d.\n" depth; bp_s, bscore) else
    match l with
    | [] -> iterative_deepening_search (depth+1) bp_s bscore moves
    | p_s::l -> 
      let score = average possible_placements p_s depth in 
        if score > bscore then iterative_deepening_search depth p_s score l
         else                  iterative_deepening_search depth bp_s bscore l in
  match moves with
  | [] -> failwith "Impossible de renvoyer le coup de meilleure moyenne puisque la liste de coups possibles est vide.\n"
  | p_s::l -> iterative_deepening_search 1 p_s (average possible_placements p_s 1) l


let rec worst (possible_placements : placement list) (p_s : pending_state) (depth : int) (worst_score : int): int =
  (* coût temporel : C(average) = longueur(possible_placements) * ( C(alphabeta) + C(play) )*)
  match possible_placements with
  | [] -> worst_score
  | pl :: l -> worst l p_s depth (min worst_score (alphabeta pl (play pl p_s) depth)) 

let worst (possible_placements : placement list) (p_s : pending_state) (depth : int): int = 
  (*Printf.printf "Appel de la fonction average : \n - possible_placements : ";
  print_list print_placement possible_placements; *)
  worst possible_placements p_s depth 0

let least_worst (possible_placements : placement list) (moves : pending_state list) : pending_state * int = 
  (* entreés : 
    - bp_s : best pending state
    - bscore : best score 
  postconditions : meilleur mouvement avec le score associé. Ils sont calculés avec alphabeta par parcours en profondeurs itérés.*)
  let end_time = Sys.time () +. search_time in
  let rec iterative_deepening_search (depth : int) (bp_s : pending_state) (bscore : int) (l : pending_state list) : pending_state * int = 
    match l with
    | [] -> if Sys.time () > end_time then (
              Printf.printf "Profondeur du calcul : %d.\n" depth; 
              bp_s, bscore
            ) else
              let p_s, l = List.hd moves, List.tl moves in 
              iterative_deepening_search (depth+1) p_s (worst possible_placements p_s 1) l
    | p_s::l -> 
      let score = worst possible_placements p_s depth in 
        if score > bscore then iterative_deepening_search depth p_s score l
          else                  iterative_deepening_search depth bp_s bscore l in
  match moves with
  | [] -> failwith "Impossible de renvoyer le coup de meilleure moyenne puisque la liste de coups possibles est vide.\n"
  | p_s::l -> iterative_deepening_search 1 p_s (worst possible_placements p_s 1) l
        


(*==========================================================================
  ===================== Interface avec l'utilisateur =======================
	==========================================================================*)
let rec user_placement (i : int) : ghost = 
  Printf.printf "De quel couleur est votre fantôme %d ? ('B' : bleu, 'R' : rouge)\n" i;
  Printf.printf "Réponse : ";
  match read_line () with
  | "B" -> B
  | "R" -> R
  | _ -> Printf.printf "Réponse incorrecte. Veuillez réessayer.\n";
          user_placement i


let rec user_move ((p, b, d) as s : state) : pending_state = 
  print_state_for_unknown s;
  Printf.printf "Que voulez-vous jouez ? (ex : 'b1b2', 'c3xd3', ...)\n";
  Printf.printf "Réponse : ";
  let explode (str : string) : char list = 
    (* post-condition : str sous forme d'une liste de caractères *)
    let rec explode_inner (i : int) (n:int) : char list = 
      if i = n then []
      else str.[i]::(explode_inner (i+1) n) in
    explode_inner 0 (String.length str) in
  assert (explode "c1c2" = ['c';'1';'c';'2']);
  let rec translate (ans : char list) (n : int) : int list =
    (* post-condition : Liste de taille n des entiers correspondant aux charactères de la liste.
        La fonction saute 'x'.
        Renvoie une exception s'il n'y a pas n caractères traduisibles.
        ex : ['b'; '1'; 'b'; '2'] -> [1;0;1;1]
              ['c'; '3'; 'x'; 'd'; '3'] -> [2;2;3;2] *)
    if n = 0 then [] else 
    match ans with
    | [] -> failwith "Coup incorrecte. Veuillez-réessayer.\n" 
    | c :: ans' -> let code = int_of_char c in 
      if n mod 2 = 0 && 97 <= code && code < 97 + board_width then
        (code - 97)::(translate ans' (n-1))
      else if n mod 2 = 1 && 48 < code && code <= 48+board_height then
        (code - 49)::(translate ans' (n-1))     
      else if c = 'x' then
        (translate ans' n)
      else
        failwith "Coup incorrecte. Veuillez-réessayer.\n" in
  assert (translate ['b'; '1'; 'b'; '2'] 4 = [1;0;1;1]);
  assert (translate ['c'; '3'; 'x'; 'd'; '3'] 4 = [2;2;3;2]);
  let ans = read_line () in 
  try 
    let i_j_list = translate (explode ans) 4 in 
    let k_from = (List.nth i_j_list 1)*board_width+(List.nth i_j_list 0) in (* Le fait que ce ne soit pas optimale d'utiliser une liste ici n'est pas grave. *)
    let k_to   = (List.nth i_j_list 3)*board_width+(List.nth i_j_list 2) in (* En effet, la liste est de taille constante (4) et cette fonction n'est appelée 
                                                                     que très rarement : quand un utilisateur entre un coup. *)
    if possible_move k_from k_to && valid_move s k_from k_to then
      create_move s k_from k_to
    else
      failwith "Coup incorrecte. Veuillez-réessayer.\n"
  with Failure message -> 
    print_string message;
    user_move s
    
(* post-condition : Appelle la fonction game_function puis affiche le résultat de la partie *)
let play_game (game_function : state -> state) (s : state) : unit = 
  let final_state = game_function s in
  print_state final_state;
  match outcome final_state with 
  | None -> Printf.printf "Egalité !\n"
  | Some p -> Printf.printf "Le gagnant est %s !\n\n" (string p)

let play_game_with_user (game_function : state -> state) (s: state) : unit =
  Printf.printf "Si vous voulez voir mon placement initial, voici mon point de vue : \n";
  print_state s;
  Printf.printf "*\n*\n*\n*\n*\n*\n*\n*\n*\n*\n*\n*\n*\n*\n*\n*\n";
  (* post-condition : Appelle la fonction game_function puis affiche le résultat de manière adaptée
      pour l'utilisateur *)
  let final_state = game_function s in
  print_state_for_unknown final_state;
  match outcome final_state with
  | None -> Printf.printf "Egalité !\n"
  | Some Known -> Printf.printf "He he, j'ai gagné !\n\n"
  | Some Unknown -> Printf.printf "Félications, vous avez gagné !\n"

  
let rec random_vs_random (pl : placement) (s: state) : state =
  match moves s with
  | [] -> s 
  | m -> 
    print_state s;
    random_vs_random pl (play pl (List.nth m (Random.int (List.length m))))

let rec random_vs_user (user_placement : placement) (s: state) : state = 
	match moves s with
	| [] -> s 
	| m -> 
		if player s = Unknown then (* L'utilisateur est le joueur inconnu pour l'ordinateur *)
			random_vs_user user_placement (play user_placement (user_move s))
		else 
			random_vs_user user_placement (play user_placement (List.nth m (Random.int (List.length m))))


let rec alphabeta_vs_user_average (user_placement : placement) (s: state) : state = 
    match moves s with
    | [] -> s
    | moves ->
      if player s = Unknown then (* L'utilisateur est le joueur inconnu pour l'ordinateur *)
        alphabeta_vs_user_average user_placement (play user_placement (user_move s))
      else
        let bmove, baverage = best_average (possible_placements s) moves in
        Printf.printf "Le coup que je joue à une moyenne de %d.\n" baverage;
        alphabeta_vs_user_average user_placement (play user_placement bmove)

let rec alphabeta_vs_user_worst (user_placement : placement) (s: state) : state = 
  match moves s with
  | [] -> s
  | moves ->
    if player s = Unknown then (* L'utilisateur est le joueur inconnu pour l'ordinateur *)
      alphabeta_vs_user_worst user_placement (play user_placement (user_move s))
    else
      let bmove, baverage = least_worst (possible_placements s) moves in
      Printf.printf "Le coup que je joue à un pire cas de %d.\n" baverage;
      alphabeta_vs_user_worst user_placement (play user_placement bmove)


let () = Printf.printf "Je m'exécute.\n"

let () = play_game_with_user (alphabeta_vs_user_worst user_placement) 
                             (let binitial_state, baverage = least_worst (all_possible_placements) all_initial_pending_states in
                               Printf.printf "L'emplacement que je joue à une moyenne de %d.\n" baverage;
                               play (random_placement ()) binitial_state)

let () = Printf.printf "Exécution terminée.\n"





