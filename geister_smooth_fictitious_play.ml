(* Méthode smooth Fictitious Play associée à IMP-Minimax pour résoudre le jeu 
Fantômes contre fantômes.
BELLIOT Raphaël
2024-2025
v1.0 *)



(* type player : Représente un joueur *)
type player = P1 | P2                               


(* type node : 
p : joueur, 
p1_pos0 : position du fantôme de P1 ayant commencé sur p1_placement_cases[0],
p1_pos1 : position du fantôme de P1 ayant commencé sur p1_placement_cases[1],
p2_pos0 : position du fantôme de P2 ayant commencé sur p2_placement_cases[0],
p2_pos1 : position du fantôme de P2 ayant commencé sur p2_placement_cases[1],
p1_0_is_blue : true ssi le fantôme en p1_pos0 est bleu
p2_0_is_blue : true ssi le fantôme en p2_pos0 est bleu
*)
type node = { 
    p: player; 
    p1_pos0: int; p1_pos1: int; 
    p2_pos0: int; p2_pos1: int; 
    p1_0_is_blue: bool; 
    p2_0_is_blue: bool;
}

(* type info_set : Représente un ensemble d'information *)
type info_set = node 
                                                    

(* type pi_set : partial information set. 
Représente un sous-ensemble d'un ensemble d'information. 
A chaque noeud est associé un poids, c'est-à-dire le produit des probabilités des 
branches sur le chemin de la racine au noeud. *)
type pi_set = (node * float) list                             


(* type alternative : (k_from, k_to) as alt. 
Représente un coup :
Si k_from >= 0 et k_to >= 0, alors alt représente un coup ordinaire : 
    (k_from, k_to) = (case de départ, case d'arrivée). 
Si k_from < 0, alors alt représente un coup de début de parti 
    qui choisit une disposition des fantômes :
    k_from = -i avec i > 0 où i-1 est l'indice de la disposition 
    des fantômes choisie dans Placement.init_possible_placements_array
Si k_from >= 0 et k_to < 0, alors alt représente un coup de début
    de partie où la disposition des fantômes est choisie aléatoirement / est 
    cachée à l'adversaire.
    Dans la suite on utilise généralement (0, -1) pour représenter un tel
    coup. *)
type alternative = int * int                        


(* type game : la liste des coups joués dans la partie, le dernier coup joué 
étant en tête de liste.
Représente une partie. *)
type game = alternative list                        


(* type strategy : Pour jouer un coup, il faut connaitre les coups précédents 
(game) et l'état actuel du jeu (info_set).
La partie seule n'est pas suffisante car les premiers coups ne renseigent pas 
sur la disposition des fantômes afin que celle-ci reste cachée à l'adversaire.
Représente une stratégie *)
type strategy = game -> info_set -> (alternative * float) list      


(* type ghost : Représente un fantôme.*)
type ghost = B | R  | EmptyCase                     


(* type set : Représente un ensemble de noeuds (états de la partie). *)
type set =  | Set of (node * float * game) list             
            | PI_Set of pi_set * game
            | Leaf of node * float * game 



(*===========================================================================
  ================================= Paramètres ==============================
  ===========================================================================*)
module Config = struct
    type config = Config3x3 | Config3x4 | Config4x4 | Config6x6
    let current_config = Config3x4

    let configuration_6x6 = (6, 6, 4, 4, [25;26;27;28;31;32;33;34], 
        [1;2;3;4;7;8;9;10], [0; 5], [30;35],  [6; 1; -6; -1])
    let configuration_4x4 = (4, 4, 1, 1, [13;14], [1;2], [0;3], [12;15], 
        [4; 1; -4; -1])
    let configuration_3x4 = (4, 3, 1, 1, [9;10], [1;2], [0;3], [8; 11], 
        [4; 1; -4; -1])
    let configuration_3x3 = (3, 3, 1, 1, [6;7], [1;2], [0], [8], 
        [3; 1; -3; -1])

    let (board_width, board_height, 
        number_of_blue_ghosts, number_of_red_ghosts, 
        p1_placement_cases, p2_placement_cases, 
        p1_exit_cases, p2_exit_cases, 
        possible_moves) = 
            match current_config with 
            | Config3x3 -> configuration_3x3
            | Config3x4 -> configuration_3x4
            | Config4x4 -> configuration_4x4 
            | Config6x6 -> configuration_6x6

    let board_length = board_width * board_height
    let number_of_ghosts = number_of_blue_ghosts + number_of_red_ghosts
    let number_of_moves = List.length possible_moves

    let () = assert (number_of_blue_ghosts + number_of_red_ghosts = 
                        List.length p1_placement_cases)
    let () = assert (number_of_blue_ghosts + number_of_red_ghosts =
                        List.length p2_placement_cases)
end





(*===========================================================================
  ================================= Utilitaires =============================
  ===========================================================================*)
module Tools = struct 
    let list_to_array (l : 'a list) : 'a array =
        (*  sortie : tableau contenant les mêmes éléments que l dans le même ordre *)
        if l = [] then [||] else
        let a = Array.make (List.length l) (List.hd l)  in
        let rec aux (l : 'a list) (i : int) : 'a array = 
            match l with
            | [] -> a
            | e :: l -> a.(i)<-e; aux l (i+1)
        in
        aux l 0

    let keys (l : ('a * 'b) list) : 'a list = 
        List.map (fun (a,b) -> a) l

    let other = function P1 -> P2 | P2 -> P1

    let get_char_at (n: node) (k: int) : char =
        if k = n.p1_pos0 then (if n.p1_0_is_blue then 'B' else 'R')
        else if k = n.p1_pos1 then (if n.p1_0_is_blue then 'C' else 'S')
        else if k = n.p2_pos0 then (if n.p2_0_is_blue then 'b' else 'r')
        else if k = n.p2_pos1 then (if n.p2_0_is_blue then 'c' else 's')
        else '.'

    let print_node (n : info_set) : unit = 
        for i = (Config.board_height-1) downto 0 do
            Printf.printf "|";
            for j = 0 to (Config.board_width-1) do
            Printf.printf " %c " (get_char_at n (i*Config.board_width+j))
            done;
            Printf.printf "|\n"
        done;
        Printf.printf "C'est à %s de jouer\n" (match n.p with | P1 ->"P1" | P2 -> "P2")
    
    let print_info_set (n : info_set) : unit = 
        for i = (Config.board_height-1) downto 0 do
            Printf.printf "|";
            for j = 0 to (Config.board_width-1) do
                let k = i*Config.board_width+j in 
                if (n.p = P1 && (k = n.p2_pos0 || k = n.p2_pos1)) || (n.p = P2 && (k = n.p1_pos0 || k = n.p1_pos1)) then
                    Printf.printf " ? "
                else
                    Printf.printf " %c " (get_char_at n k)
            done;
            Printf.printf "|\n"
        done;
        Printf.printf "C'est à %s de jouer\n" (match n.p with | P1 ->"P1" | P2 -> "P2")

    let print_list (l : 'a list) (print_fn : 'a -> unit) : unit = 
        (* affiche la liste donnée en entrée, chacun des éléments étant affiché 
            à l'aide de print_fn.*)
        Printf.printf "[";
        let rec aux (l : 'a list) : unit = 
            match l with 
            | [] -> Printf.printf "]"
            | [elt] -> print_fn elt; Printf.printf "]"
            | elt::l' -> print_fn elt; Printf.printf ";"; aux l'
        in 
        aux l

    let shuffle_list (l : 'a list) : 'a list=
        (* sortie : la liste mélangée en O(n) grâce à l'algorithme de 
            Fisher-Yates *)
        let arr = Array.of_list l in
        let n = Array.length arr in
        for i = n - 1 downto 1 do
            let j = Random.int (i + 1) in
            let tmp = arr.(i) in
            arr.(i) <- arr.(j);
            arr.(j) <- tmp;
        done;
        Array.to_list arr

    let print_game (game : game) = 
        print_list game 
            (fun (k_from, k_to)-> Printf.printf "(%d, %d)" k_from k_to)

    let print_games (games : game list) = 
        print_list games print_game; print_newline ()
    

    let compare_scores (score1 : float) (score2 : float) (epsilon : float) : 
            bool = 
        (* sortie : |score1 - score2| <= epsilon *)
        abs_float (score1 -. score2) <= epsilon

    let print_set (x : set) : unit = 
        match x with
        | Set l-> Printf.printf "Set (";
            print_list l (fun (n, w, g) -> 
            Printf.printf "(\n";
            print_node n;
            Printf.printf ", \n%f, \n" w;
            print_game g;
            Printf.printf ")")
        | _ -> ()
end      





(*===========================================================================
  ======== Fonctions et algorithmes sur les dispositions des fantômes =======
  ===========================================================================*)
module Placement = struct
    (* Type auxiliaire permettant de créer des placements de manière récursive 
    Généralement, le fantôme d'indice i dans la liste correspond au fantôme inconnu à placer sur la case i-ème case de départ, ou provenant 
    de la i-ème case de départ.*) 
    type placement = int -> ghost 
    type list_placement = ghost list


    let list_placement_to_placement (l_p : list_placement) : placement = 
        let a = Tools.list_to_array l_p in
            (fun i -> a.(i))


    let () = Random.self_init () (* Initialisation du module Random, autrement il donne toujours les mêmes valeurs
                                    au fil des éxécutions.*)
    let random_placement_list () : list_placement = 
        let rec aux (blues : int) (reds : int) : list_placement = 
            match (blues, reds) with
            | 0, 0 -> []
            | 0, _ -> R::aux blues (reds-1)
            | _, 0 -> B::aux (blues-1) reds
            (* On pioche R proportionnellement au nombre de R restants,
               garantissant une vraie distribution uniforme mathématiquement sur toutes les permutations (O(N)). *)
            | _, _ -> if Random.int (blues + reds) < reds then
                        R::aux blues (reds-1)
                    else
                        B::aux (blues-1) reds 
        in aux Config.number_of_blue_ghosts Config.number_of_red_ghosts

    let random_placement () : placement = 
        list_placement_to_placement (random_placement_list ())

    
    let possible_list_placements (number_of_blue_ghosts, number_of_red_ghosts : int * int): list_placement list =
        (* post-condition : Une liste contenant tous les dispositions possibles à tant de fantômes sous forme de listes.*)        
        let rec aux (i : int) (l_p : list_placement) (blues : int) (reds : int) (acc : list_placement list ): list_placement list = 
            (* précondition : - -1 <= i < number_of_blue_ghosts + number_of_red_ghosts *)
            if i = -1 then 
                l_p::acc 
            else
                if blues = number_of_blue_ghosts then
                    aux (i-1) (R::l_p) blues (reds+1) acc
                else if reds = number_of_red_ghosts then
                    aux (i-1) (B::l_p) (blues+1) reds acc
                else
                    aux (i-1) (R::l_p) blues (reds+1) 
                        (aux (i-1)  (B::l_p) (blues+1) reds acc) in
        
        aux (number_of_blue_ghosts+number_of_red_ghosts-1) [] 0 0 []
    
    let possible_placements (number_of_blue_ghosts, number_of_red_ghosts : int * int): placement list = 
        (* post-condition : Une liste contenant tous les dispositions possibles à tant de fantômes. *)
        List.map list_placement_to_placement (possible_list_placements (number_of_blue_ghosts,number_of_red_ghosts))
        
    let init_possible_placements = possible_placements (Config.number_of_blue_ghosts, Config.number_of_red_ghosts)

    let init_possible_placements_array = (* Attention ! C'est un tableau ! (pour pouvoir y accéder en O(1) dans descend) *)
        Tools.list_to_array (init_possible_placements)

    let number_of_init_possible_placements = Array.length init_possible_placements_array

    let add_init_placement (n : node) (pl : placement) : node = 
        match n.p with 
        | P1 -> 
            let p1c = Config.p1_placement_cases in
            let p1_pos0, p1_pos1 = (List.nth p1c 0, List.nth p1c 1) in
            let p1_0_is_blue = (pl 0 = B) in
            { p = P2;
              p1_pos0; p1_pos1; 
              p2_pos0 = n.p2_pos0; p2_pos1 = n.p2_pos1; 
              p1_0_is_blue; 
              p2_0_is_blue = n.p2_0_is_blue }
        | P2 -> 
            let p2c = Config.p2_placement_cases in
            let p2_pos0, p2_pos1 = (List.nth p2c 0, List.nth p2c 1) in
            let p2_0_is_blue = (pl 0 = B) in
            { p = P1;
              p1_pos0 = n.p1_pos0; p1_pos1 = n.p1_pos1; 
              p2_pos0; p2_pos1; 
              p1_0_is_blue = n.p1_0_is_blue; 
              p2_0_is_blue }


    let is_placement_node (n : node) : bool = 
        (* Sortie : true ssi le noeud donné argument est la racine ou un fils de la racine, 
            i.e, un noeud où les alternatives sont des placements et non des mouvements de fantôme.*)
        (n.p1_pos0 = -1 && n.p1_pos1 = -1) || (n.p2_pos0 = -1 && n.p2_pos1 = -1)


    let print_placement (n : node) (pl : placement) : unit = 
        let updated_n = add_init_placement n pl in (* on ajoute le placement donc il pense que c'est à l'adversaire de jouer *)
        let updated_n = { updated_n with p = Tools.other updated_n.p } in
        Tools.print_info_set updated_n

end





module GameTree = struct 

    let descend (k_from, k_to : alternative) (n : node): node = 
        (* sortie : renvoie le noeud dérivant du noeud n en jouant a *)
        if k_from < 0 then 
            Placement.add_init_placement n Placement.init_possible_placements_array.(-k_from-1)
        else if k_to < 0 then 
            Placement.add_init_placement n Placement.init_possible_placements_array.(Random.int Placement.number_of_init_possible_placements)
        else 
            let p1_pos0' = if n.p1_pos0 = k_from then k_to else (if n.p1_pos0 = k_to then -1 else n.p1_pos0) in
            let p1_pos1' = if n.p1_pos1 = k_from then k_to else (if n.p1_pos1 = k_to then -1 else n.p1_pos1) in
            let p2_pos0' = if n.p2_pos0 = k_from then k_to else (if n.p2_pos0 = k_to then -1 else n.p2_pos0) in
            let p2_pos1' = if n.p2_pos1 = k_from then k_to else (if n.p2_pos1 = k_to then -1 else n.p2_pos1) in
            { p = Tools.other n.p; 
              p1_pos0 = p1_pos0'; p1_pos1 = p1_pos1'; 
              p2_pos0 = p2_pos0'; p2_pos1 = p2_pos1'; 
              p1_0_is_blue = n.p1_0_is_blue; p2_0_is_blue = n.p2_0_is_blue }

    (* Premiers noeuds de l'arbre en Config4x4 *)
    let root = { p = P1; p1_pos0 = -1; p1_pos1 = -1; p2_pos0 = -1; p2_pos1 = -1; p1_0_is_blue = true; p2_0_is_blue = true }
    let n1 = descend (-1, 0) root
    let n2 = descend (-2, 0) root
    let n11 = descend (-1, 0) n1
    let n12 = descend (-2, 0) n1
    let n21 = descend (-1, 0) n2
    let n22 = descend (-2, 0) n2

    (* Début de partie où les deux joueurs ne connaissent pas la disposition des fantômes adverses.*)
    let random_beginning = [(0, -1);(0, -1)]


    let on_the_board (k_from : int) (k_to : int) : bool = 
        (* sortie : true si le coups joué ne sort pas du plateau, false sinon*)
        k_to >= 0 && k_to < Config.board_length 
        && not (k_from mod Config.board_width = 0 && k_to - k_from = -1) 
        && not (k_from mod Config.board_width = (Config.board_width-1) && k_to - k_from = 1) 

    let is_leaf (n : node) : bool = 
        (*  entrée : un noeud
            sortie : true ssi le noeud est une feuille, i.e la partie est terminée dans ce noeud.  *)
        let pos_b1, pos_r1 = if n.p1_0_is_blue then n.p1_pos0, n.p1_pos1 else n.p1_pos1, n.p1_pos0 in
        let pos_b2, pos_r2 = if n.p2_0_is_blue then n.p2_pos0, n.p2_pos1 else n.p2_pos1, n.p2_pos0 in
        List.mem pos_b1 Config.p1_exit_cases || 
        List.mem pos_b2 Config.p2_exit_cases || 
        (pos_b1 < 0 && pos_r1 >= 0) || 
        (pos_r1 < 0 && pos_b1 >= 0) || 
        (pos_b2 < 0 && pos_r2 >= 0) || 
        (pos_r2 < 0 && pos_b2 >= 0) 


    let alternatives (x_set : pi_set) : alternative list = 
        (* sortie : la liste des coups possibles depuis un ensemble d'information partielle.*)

        let n = fst (List.hd x_set) in (* Un seul noeud du PI-Set suffit 
                                        pour déterminer les coups jouables puisque chacun 
                                        des noeuds ont les mêmes coups possibles étant donné
                                        qu'ils appartiennent au même PI-Set *)
        if is_leaf n then [] (* partie terminée -> pas de coups possibles *)
        else if Placement.is_placement_node n then   (* début de partie -> les coups possibles sont des dispositions de fantômes.*)
            List.init (Placement.number_of_init_possible_placements) (fun i -> (-i-1, 0))
        else
        let p_ghosts = if n.p = P1 then [n.p1_pos0; n.p1_pos1] else [n.p2_pos0; n.p2_pos1] in
        let all_p_ghosts = List.filter (fun x -> x >= 0) p_ghosts in
        
        List.fold_left (fun acc k_from ->
            List.fold_left (fun acc' move -> 
                        let k_to = k_from + move in
                let target_is_friendly = if n.p = P1 then (k_to = n.p1_pos0 || k_to = n.p1_pos1) else (k_to = n.p2_pos0 || k_to = n.p2_pos1) in
                if on_the_board k_from k_to && not target_is_friendly then (k_from, k_to)::acc'
                else acc'
            ) acc Config.possible_moves
        ) [] all_p_ghosts

    let convert_to_info_set (x_set : pi_set) : info_set = 
        (* précondition : x_set est non vide et x_set est un ensemble d'informations partiel pour le joueur dont c'est le tour *)
        (* Un info set est simplement un noeud où les fantômes adverses ne sont pas forcément de la couleur renseignée *)
        fst (List.hd x_set)

    let pi_set_player (x_pi_set : pi_set) : player = 
        (* sortie : le joueur dont c'est le tour *)
        match x_pi_set with 
        | [] -> failwith "Impossible de déterminer le joueur d'un ensemble d'information partielle vide."
        | ({p; _}, _)::_ -> p





    let payoff (p : player) (n : node) : float = 
        (* précondition : le noeud est une feuille 
        sortie : -1 si le joueur donné perd, 1 s'il gagne, 0 sinon*)

        (* On désambiguise une règle : si un fantôme bleu sort en mangeant un fantôme rouge, le fantôme bleu gagne. *)
        let pos_b1, pos_r1 = if n.p1_0_is_blue then n.p1_pos0, n.p1_pos1 else n.p1_pos1, n.p1_pos0 in
        let pos_b2, pos_r2 = if n.p2_0_is_blue then n.p2_pos0, n.p2_pos1 else n.p2_pos1, n.p2_pos0 in
        if List.mem pos_b1 Config.p1_exit_cases then 
            if p = P1 then 1. else -1.
        else if List.mem pos_b2 Config.p2_exit_cases then
            if p = P1 then -1. else 1.
        else if pos_b2 < 0 || pos_r1 < 0 then 
            if p = P1 then 1. else -1.
        else if pos_b1 < 0 || pos_r2 < 0 then
            if p = P1 then -1. else 1.
        else
            0.

    let opg_children (avg_opp_str : strategy) (x_set : pi_set) (g : game) : (set * alternative) list = 
        (* One player game children 
        pré-condition : x_set est non vide, ne contient pas deux noeuds identiques
        sortie : la liste des enfants de l'ensemble x_set. Un enfant est un ensemble d'ensembles à informations imparfaites
            directement en dessous de x_set dans l'arbre à un joueur via une coup précis.*)

        (* Etape 1 : on génère les pi_sets découlant des alternatives possibles pour le joueur à qui c'est le tour *)
        let pi_sets_games_alts = List.map (fun alt -> ((List.map (fun (n, w) -> descend alt n, w) x_set),
                                                        (if fst alt < 0 then (0, -1)::g else alt::g), alt)) (alternatives x_set) in 

        (* Etape 2 : on transforme ces pi_sets en information imparfaite pour le joueur adverse *)
        (* Rien à faire : chacun des noeuds d'un pi_set de pi_sets_alts représente déjà un ensemble d'information différent 
        pour le joueur adverse
        En effet, soit un ensemble d'information partiel x de pi_sets_alts. On sait qu'il n'y pas deux noeuds identiques dans x (précondition)
        Donc toutes les noeuds représentent des dispositions de fantômes de l'adversaire différentes. Donc les noeuds de x représentent 
        chacun un ensemble d'information différent pour l'adversaire. *)

        (* Etape 3 : on récupère chacun des ensembles (qui ne sont plus des ensembles d'information) 
        découlant de l'alternative choisipi_setse par le joueur adverse sous la stratégie pi *)
        List.fold_left (fun acc (pi_set, g, alt) -> 
                    (Set (List.fold_left  
                        (fun acc (n, w) -> 
                            if is_leaf n then 
                                (n, w, g)::acc
                            else
                                let opp_alts = alternatives [(n, w)] in
                                let info_set_n = convert_to_info_set [(n, w)] in
                                let opp_strat_alt_probs = avg_opp_str g info_set_n in
                                
                                List.fold_left (fun sub_acc opp_alt -> 
                                    let prob_w = List.assoc opp_alt opp_strat_alt_probs 
                                    in
                                    (
                                        descend opp_alt n, 
                                        w *. prob_w,
                                        if fst opp_alt < 0 then (0, -1)::g else opp_alt::g
                                    ) :: sub_acc
                                ) acc opp_alts
                            ) 
                        [] pi_set
                        ), alt)::acc
                ) [] pi_sets_games_alts

    let tpg_children (x_set : pi_set) (g : game) : (set * alternative) list = 
        (* Two player game children 
        pré-condition : x_set est non vide, ne contient pas deux noeuds identiques
        post-condition : la liste des enfants de l'ensemble x_set. Un enfant est un ensemble d'ensembles à informations imparfaites
            directement en dessous de x_set dans l'arbre à deux joueurs via une coup précis.*)

        (* Etape 1 : on génère les ensembles de noeuds découlant des alternatives possibles pour le joueur à qui c'est le tour *)
        List.map (fun alt -> (Set (List.map (fun (n, w) -> descend alt n, w, alt::g) x_set), alt)) (alternatives x_set)

end




module Encoding = struct 
    let encode (n : info_set) : int = 
        (* précondition : on est en config4x4
        sortie : on code sur 22 bits l'ensemble d'information qui représente un état du jeu : 
            - 4 premiers bits = position (indice k) fantôme bleu connu
            - 4 bits suivants = position fantôme rouge connu 
            - 4 bits suivant : position du fantôme inconnu provenant de la case de départ n°0
            - 4 bits suivant : position du fantôme inconnu provenant de la case de départ n°1
            - bit suivant : b1 (vaut 1 si le fantôme bleu de P1 est en vie, 0 sinon)
            - bit suivant : r1
            - bit suivant : b2
            - bit suivant : r2
            - bit de fin : p (0 si p = P1 et 1 si p = P2)
        (dans le sens inverse) *)
        
        let p1_pos0 = ref (max 0 n.p1_pos0) in
        let p1_pos1 = ref (max 0 n.p1_pos1) in 
        let p2_pos0 = ref (max 0 n.p2_pos0) in
        let p2_pos1 = ref (max 0 n.p2_pos1) in 
        
        let b1 = (if n.p1_pos0 >= 0 && n.p1_0_is_blue then 1 else 0) + (if n.p1_pos1 >= 0 && not n.p1_0_is_blue then 1 else 0) in
        let r1 = (if n.p1_pos0 >= 0 && not n.p1_0_is_blue then 1 else 0) + (if n.p1_pos1 >= 0 && n.p1_0_is_blue then 1 else 0) in
        let b2 = (if n.p2_pos0 >= 0 && n.p2_0_is_blue then 1 else 0) + (if n.p2_pos1 >= 0 && not n.p2_0_is_blue then 1 else 0) in
        let r2 = (if n.p2_pos0 >= 0 && not n.p2_0_is_blue then 1 else 0) + (if n.p2_pos1 >= 0 && n.p2_0_is_blue then 1 else 0) in
        
        (* Préciser via la parité de (p1_pos0 + p1_pos1) où se trouve le fantôme vivant s'il n'y en a plus qu'un pour P1 *)
        if b1 + r1 = 1 then begin
            if n.p1_pos0 >= 0 then p1_pos1 := !p1_pos0 mod 2             (* p1_pos0 + p1_pos1 = pair *)
            else p1_pos0 := 1 + (!p1_pos1 mod 2)                         (* p1_pos0 + p1_pos1 = impair *)
        end;

        (* De même pour P2 *)
        if b2 + r2 = 1 then begin
            if n.p2_pos0 >= 0 then p2_pos1 := !p2_pos0 mod 2             (* p2_pos0 + p2_pos1 = pair *)
            else p2_pos0 := 1 + (!p2_pos1 mod 2)                         (* p2_pos0 + p2_pos1 = impair *)
        end;

        !p1_pos0 lor 
        (!p1_pos1 lsl 4) lor 
        (!p2_pos0 lsl 8) lor
        (!p2_pos1 lsl 12) lor
        (b1 lsl 16) lor
        (r1 lsl 17) lor
        (b2 lsl 18) lor
        (r2 lsl 19) lor
        ((match n.p with | P1 -> 0 | P2 -> 1) lsl 20) lor
        ((if (match n.p with | P1 -> n.p1_0_is_blue | P2 -> n.p2_0_is_blue) then 1 else 0) lsl 21)


end 





module Strategy = struct 
    let create_uniform_strategy () : strategy = 
        fun g i -> 
        let alts = GameTree.alternatives [(i, 1.)] in 
        let n = List.length alts in 
        List.map (fun alt -> (alt, 1. /. (float n))) alts

    let create_strategy (h : ((game * int), (alternative * float) list) Hashtbl.t) (last_str : strategy) : strategy = 
        (* sortie : créer une stratégie qui renvoie le coup de h si l'état (g,i) est 'dans' h et 
            renvoie le coup de last_str sinon *)
        fun (g : game) (i : info_set) -> 
            match Hashtbl.find_opt h (g, Encoding.encode i) with 
            | None -> last_str g i
            | Some alt_probs -> alt_probs

    let create_random_strategy () : strategy = 
        let h = Hashtbl.create (1 lsl 15) in 
        fun (g : game) (i : info_set) -> 
            let e = Encoding.encode i in 
            match Hashtbl.find_opt h (g, e) with 
            | None -> 
                let alts = GameTree.alternatives [(i, 1.)] in 
                let alt_probs =
                let weights = List.map (fun alt -> (alt, Random.float 1.0)) alts in
                let total = List.fold_left (fun acc (_, w) -> acc +. w) 0. weights in
                List.map (fun (alt, w) -> (alt, w /. total)) weights
                in begin
                Hashtbl.add h (g,e) alt_probs;
                alt_probs
            end 
            | Some alt_probs -> alt_probs

    let create_random_pure_strategy () : strategy = 
        let h = Hashtbl.create (1 lsl 15) in 
        fun (g : game) (i : info_set) -> 
            let e = Encoding.encode i in 
            match Hashtbl.find_opt h (g, e) with 
            | None -> 
                let alts = GameTree.alternatives [(i, 1.)] in 
                let alt_probs =
                let chosen = List.nth alts (Random.int (List.length alts)) in
                List.map (fun alt -> (alt, if alt = chosen then 1. else 0.)) alts
                in begin
                Hashtbl.add h (g,e) alt_probs;
                alt_probs
            end 
            | Some alt_probs -> alt_probs

    let convert_to_pure_strategy (str : strategy) : strategy = 
        fun (g :game) (i : info_set) -> 
            match str g i with 
            | [] -> failwith "str g i est une liste vide."
            | (alt, prob)::alt_probs' as alt_probs ->
                let highest_alt, highest_prob = 
                    List.fold_left (fun (highest_alt, highest_prob) (alt, prob) -> 
                                        if prob > highest_prob then 
                                            (alt, prob) 
                                        else (highest_alt, highest_prob))
                                    (alt, prob) alt_probs' in 
                List.map (fun (alt, prob) -> if alt = highest_alt then (alt, 1.) else (alt, 0.)) alt_probs

end 


(* ============================================================
   MODULE StrategyIO — Sauvegarde et chargement de stratégies
   ============================================================ *)
module StrategyIO = struct

    (* Nom du fichier de données binaire (Marshal) *)
    let dat_filename (strat_name : string) : string =
        let version = Sys.ocaml_version in
        Printf.sprintf "%s_hashtbl_ocamlv%s.dat" strat_name version

    (* [save_hashtbl_strategy h strat_name]
       Sauvegarde la table de hachage [h] dans un fichier binaire via Marshal,
       puis génère un fichier .ml "wrapper" qui recharge cette table à l'exécution
       et expose une fonction [alt_probs : game -> info_set -> (alternative * float) list].

       Portabilité :
       - Le flag [Marshal.Compat_32] est activé pour maximiser la compatibilité
         entre architectures 32 et 64 bits.
       - Le fichier .dat encode la version d'OCaml dans son nom : le wrapper
         généré affiche un avertissement si la version courante ne correspond pas.
       - Pour charger la stratégie sur une autre machine, copier ensemble
         le fichier .dat ET le fichier .ml généré.
    *)
    let save_hashtbl_strategy
            (h : ((game * int), (alternative * float) list) Hashtbl.t)
            (strat_name : string) : unit =
        let dir = "strategies" in
        if not (Sys.file_exists dir) then ignore (Sys.command (Printf.sprintf "mkdir -p %s" dir));
        let ocaml_version = Sys.ocaml_version in
        let dat_basename = dat_filename strat_name in
        let dat_file = dir ^ "/" ^ dat_basename in
        let ml_file  = dir ^ "/" ^ strat_name ^ ".ml" in

        (* 1. Sauvegarder la Hashtbl en binaire *)
        let out = open_out_bin dat_file in
        Marshal.to_channel out h [Marshal.Compat_32];
        close_out out;
        Printf.printf "[StrategyIO] Table sauvegardée dans '%s'.\n" dat_file;

        (* 2. Générer le wrapper .ml *)
        let oc = open_out ml_file in
        Printf.fprintf oc "(* ============================================================\n";
        Printf.fprintf oc "   Fichier généré automatiquement par StrategyIO.save_hashtbl_strategy\n";
        Printf.fprintf oc "   Stratégie : %s\n" strat_name;
        Printf.fprintf oc "   Version OCaml utilisée lors de la génération : %s\n" ocaml_version;
        Printf.fprintf oc "\n";
        Printf.fprintf oc "   IMPORTANT : Ce fichier doit être utilisé avec OCaml %s.\n" ocaml_version;
        Printf.fprintf oc "   Le fichier de données binaires associé est : %s\n" dat_file;
        Printf.fprintf oc "   Si vous utilisez une version d'OCaml différente, le chargement\n";
        Printf.fprintf oc "   du fichier .dat échouera (format Marshal non portable entre versions).\n";
        Printf.fprintf oc "   ============================================================ *)\n";
        Printf.fprintf oc "\n";
        Printf.fprintf oc "(* Chemin vers le fichier de données — à adapter si déplacé. *)\n";
        Printf.fprintf oc "let _dat_file = \"%s\"\n" dat_file;
        Printf.fprintf oc "let _expected_ocaml_version = \"%s\"\n" ocaml_version;
        Printf.fprintf oc "\n";
        Printf.fprintf oc "let () =\n";
        Printf.fprintf oc "  if Sys.ocaml_version <> _expected_ocaml_version then\n";
        Printf.fprintf oc "    Printf.eprintf\n";
        Printf.fprintf oc "      \"[AVERTISSEMENT] Strategie '%s' compilee avec OCaml %s\\n\"\n" strat_name ocaml_version;
        Printf.fprintf oc "      |> ignore;\n";
        Printf.fprintf oc "    Printf.eprintf\n";
        Printf.fprintf oc "      \"mais vous utilisez OCaml %%s. Le fichier .dat risque d'etre incompatible.\\n\"\n";
        Printf.fprintf oc "      Sys.ocaml_version\n";
        Printf.fprintf oc "\n";
        Printf.fprintf oc "(* Chargement de la table de hachage depuis le fichier binaire. *)\n";
        Printf.fprintf oc "let _h : ((game * int), (alternative * float) list) Hashtbl.t =\n";
        Printf.fprintf oc "  let ic = open_in_bin _dat_file in\n";
        Printf.fprintf oc "  let tbl = (Marshal.from_channel ic\n";
        Printf.fprintf oc "    : ((game * int), (alternative * float) list) Hashtbl.t) in\n";
        Printf.fprintf oc "  close_in ic; tbl\n";
        Printf.fprintf oc "\n";
        Printf.fprintf oc "(* Strategie de secours : distribution uniforme sur les alternatives. *)\n";
        Printf.fprintf oc "let _uniform (g : game) (i : info_set) : (alternative * float) list =\n";
        Printf.fprintf oc "  ignore g;\n";
        Printf.fprintf oc "  let alts = GameTree.alternatives [(i, 1.)] in\n";
        Printf.fprintf oc "  let n = List.length alts in\n";
        Printf.fprintf oc "  List.map (fun alt -> (alt, 1. /. float n)) alts\n";
        Printf.fprintf oc "\n";
        Printf.fprintf oc "(** [alt_probs g i] renvoie la distribution de probabilites sur les\n";
        Printf.fprintf oc "    alternatives disponibles dans l'etat d'information [i] apres la\n";
        Printf.fprintf oc "    partie [g]. Utilise la table precalculee ; retombe sur la\n";
        Printf.fprintf oc "    distribution uniforme si l'etat n'a pas ete explore. *)\n";
        Printf.fprintf oc "let alt_probs (g : game) (i : info_set) : (alternative * float) list =\n";
        Printf.fprintf oc "  match Hashtbl.find_opt _h (g, Encoding.encode i) with\n";
        Printf.fprintf oc "  | Some probs -> probs\n";
        Printf.fprintf oc "  | None       -> _uniform g i\n";
        close_out oc;
        Printf.printf "[StrategyIO] Wrapper OCaml généré dans '%s'.\n" ml_file

    (* [load_hashtbl_strategy dat_file]
       Recharge une Hashtbl sauvegardée via [save_hashtbl_strategy].
       Utile pour reconstruire une stratégie directement en mémoire
       sans passer par le fichier wrapper .ml.
       Exemple : let h = StrategyIO.load_hashtbl_strategy "hashtbl_bot1_ocamlv5.4.1.dat"
                 let bot1 = Strategy.create_strategy h (Strategy.create_uniform_strategy ()) *)
    let load_hashtbl_strategy (dat_file : string)
            : ((game * int), (alternative * float) list) Hashtbl.t =
        let ic = open_in_bin dat_file in
        let tbl = (Marshal.from_channel ic
            : ((game * int), (alternative * float) list) Hashtbl.t) in
        close_in ic;
        Printf.printf "[StrategyIO] Table chargée depuis '%s'.\n" dat_file;
        tbl

end




module UserInterface = struct 

    let rec user_alternative (n : node) : alternative = 
        (* demande à l'utilisateur de rentrer un coup, puis renvoie ce coup s'il est correct, sinon
            elle s'appelle récursivement. *)
        let explode (str : string) : char list = 
            (* sortie : str sous forme d'une liste de caractères *)
            let rec explode_inner (i : int) (n:int) : char list = 
            if i = n then []
            else str.[i]::(explode_inner (i+1) n) in
            explode_inner 0 (String.length str) in
        
        let rec translate (ans : char list) (n : int) : int list =
            (* sortie : Liste de taille n des entiers correspondant aux charactères de la liste.
                La fonction saute 'x'.
                Renvoie une exception s'il n'y a pas n caractères traduisibles.
                ex : ['b'; '1'; 'b'; '2'] -> [1;0;1;1]
                    ['c'; '3'; 'x'; 'd'; '3'] -> [2;2;3;2] *)
            if n = 0 then [] else 
            match ans with
            | [] -> failwith "" 
            | c :: ans' -> let code = int_of_char c in 
            if n mod 2 = 0 && 97 <= code && code < 97 + Config.board_width then
                (code - 97)::(translate ans' (n-1))
            else if n mod 2 = 1 && 48 < code && code <= 48+Config.board_height then
                (code - 49)::(translate ans' (n-1))     
            else if c = 'x' then
                (translate ans' n)
            else
                failwith "" in
        
        try 
            if Placement.is_placement_node n then begin 
                Printf.printf "Quelle disposition de fantômes choisissez-vous ?\n";
                for j = 0 to Array.length Placement.init_possible_placements_array -1 do 
                    Printf.printf "Numéro %d : \n" (j + 1);
                    Placement.print_placement n Placement.init_possible_placements_array.(j);
                done;
                let ans = try read_int () with Failure _ -> failwith "Veuillez renseigner un chiffre.\n" in 
                if 0 < ans && ans <= Array.length Placement.init_possible_placements_array then 
                    (-ans, 0)
                else 
                    failwith "Ce chiffre ne correspond à aucune des dispostions possibles.\n"
            end else begin
                Printf.printf "Que voulez-vous jouez ? (ex : 'b1b2', 'c3xd3', ...)\n";
                Printf.printf "Réponse : ";
                let ans = read_line () in 
                let i_j_list = translate (explode ans) 4 in 
                let k_from = (List.nth i_j_list 1)*Config.board_width+(List.nth i_j_list 0) in (* Le fait que ce ne soit pas optimale d'utiliser une liste ici n'est pas grave. *)
                let k_to   = (List.nth i_j_list 3)*Config.board_width+(List.nth i_j_list 2) in (* En effet, la liste est de taille constante (4) et cette fonction n'est appelée 
                                                                                que très rarement : quand un utilisateur entre un coup. *)
                let target_is_friendly = if n.p = P1 then (k_to = n.p1_pos0 || k_to = n.p1_pos1) else (k_to = n.p2_pos0 || k_to = n.p2_pos1) in
                let from_is_friendly = if n.p = P1 then (k_from = n.p1_pos0 || k_from = n.p1_pos1) else (k_from = n.p2_pos0 || k_from = n.p2_pos1) in
                if GameTree.on_the_board k_from k_to && from_is_friendly && not target_is_friendly then
                    (k_from, k_to)
                else
                    failwith "Les règles n'autorisent pas ce coup.\n"
            end
        with Failure message ->
            print_string message;
            user_alternative n
    
    exception IncorrectAnswer 

    let pick_alternative (alt_probs : (alternative * float) list) : alternative = 
      let r = Random.float 1. in 
      let rec pick (alt_probs : (alternative * float) list) (r : float) : alternative = 
        match alt_probs with
        | [] -> Printf.printf "Pick_alternative : r = %f\n" r; failwith "alt_probs n'est pas correcte."
        | (alt, w)::alt_probs' -> 
            if r <= w then 
              alt 
            else 
              pick alt_probs' (r-.w) in 
      pick alt_probs r

    let rec user_against_strategy (pi : strategy) (user_player : player) : unit = 
        (* L'utilisateur joue avec le joueur user_player contre la stratégie mixte pi *)
        try
        Printf.printf "Voulez-voir la couleur des fantômes adverses ? ('1' : oui, '0': non) \n";
        let visible = 
            try read_int () == 1 with Failure _ -> raise IncorrectAnswer in
        let rec play_game (n : node) (g : game): unit = 
            if GameTree.is_leaf n then (
                Tools.print_node n;
                Printf.printf "\n";
                Tools.print_game g;
                Printf.printf "\n";
                match n.p, GameTree.payoff n.p n with 
                | P1, 1. | P2, -1. -> Printf.printf "Le joueur P1 gagne la partie !\n"
                | P1, -1. | P2, 1. -> Printf.printf "Le joueur P2 gagne la partie !\n"
                | _ -> Printf.printf "Il y a égalité... Ce n'est pas possible dans ce jeu donc il y a une erreur dans le programme !\n"
            ) else 
                let alt = 
                    if n.p = user_player then (
                        if visible then 
                            Tools.print_node n
                        else 
                            Tools.print_info_set n;
                        user_alternative n
                    ) else
                        let alt_probs = (pi g n) in 
                        Tools.print_list alt_probs (fun (alt, prob) -> Printf.printf "((%d,%d), %f)" (fst alt) (snd alt) prob);
                        print_newline ();
                        pick_alternative alt_probs in 
                let g' = if fst alt < 0 then (0, -1)::g else alt::g in (* Il ne faut pas dévoiler la disposition des fantômes, c'est
                                                                        pourquoi l'on met (0, -1) dans les coups joués au début. *)
                play_game (GameTree.descend alt n) g' in 
        play_game GameTree.root []

        with IncorrectAnswer -> 
            Printf.printf "Veuillez renseigner '0' ou '1'.\n"; 
            user_against_strategy pi user_player

    let strategy_against_strategy (p1_str : strategy) (p2_str : strategy) (depth : int): float = 
        (* Deux stratégies (pures ou mixtes) s'affrontent avec un nombre de coups (total, pas chacun) limité à depth.
        Sortie : Si la partie dépasse depth coups, il y a égalité : 0.0 est renvoyé. Sinon 1.0 pour P1 gagnant et 
        -1.0 pour P1 perdant est renvoyé *)
        let rec play_game (n : node) (cnt : int) (g: game): float = 
            if GameTree.is_leaf n then 
                GameTree.payoff P1 n
            else if cnt >= depth then 
                0.0
            else 
                let alt = pick_alternative (match n.p with P1 -> p1_str g n | P2 -> p2_str g n) in 
                let g' = if fst alt < 0 then (0, -1)::g else alt::g in
                play_game (GameTree.descend alt n) (cnt+1) g' in 
        play_game GameTree.root 0 []

    let show_strategy_against_strategy (p1_str : strategy) (p2_str : strategy) (depth : int) : float = 
        (* C'est exactement la même fonction que strategy_against_strategy, mais celle-ci affiche l'état du jeu entre 
            chaque coup. *)
        let rec play_game (n : node) (cnt : int) (g : game): float = 
            Tools.print_node n;
            Printf.printf "\n";
            Tools.print_game g;
            Printf.printf "\n";
            if GameTree.is_leaf n then 
                GameTree.payoff P1 n 
            else if cnt >= depth then
                0.0
            else 
                let alt_probs = match n.p with P1 -> p1_str g n | P2 -> p2_str g n in 
                Tools.print_list alt_probs (fun (alt, prob) -> Printf.printf "((%d,%d), %f)" (fst alt) (snd alt) prob);
                print_newline ();
                let alt = pick_alternative (alt_probs) in 
                let g' = if fst alt < 0 then (0, -1)::g else alt::g in
                play_game (GameTree.descend alt n) (cnt+1) g' in 
        play_game GameTree.root 0 []
    
    let test_strategies (p1_str : strategy) (p2_str: strategy) (nb_simul : int) (depth : int) : float = 
        (*  Fait s'affronter les stratégies p1_str et p2_str nb_simul fois
            Sortie : le score de P1, i.e. la somme des résultats pour P1 divisé par nb_simul. *)
        let score = ref 0. in 
        for cnt = 1 to nb_simul do 
            score := !score +. (strategy_against_strategy p1_str p2_str depth)
        done;
        !score /. (float nb_simul)
end





module IMP_MINIMAX = struct

    let partition (x_set : set) : set list * set list = 
        (*  entrée : x un ensemble de noeuds 
            sortie : un ensemble de pi_set, un ensemble de feuilles. 
                L'union des pi_sets et de l'ensemble des feuilles 
                vaut l'ensemble x_set. Les pi_sets appartiennent à des 
                ensembles d'information deux à deux disjoints. *)
        
        match x_set with 
        | PI_Set _    -> [], [x_set]
        | Leaf _      -> [x_set], []
        | Set l       -> 
            let hash_table = Hashtbl.create Config.number_of_moves in 
            List.iter (fun (n, w, g) -> 
                let c = Encoding.encode n in 
                match Hashtbl.find_opt hash_table (g,c) with 
                | None -> Hashtbl.add hash_table (g,c) [(n, w)]
                | Some (nodes_lst) -> Hashtbl.replace hash_table (g,c) 
                                                        ((n, w)::nodes_lst)
            ) l;
            Hashtbl.fold (fun (g,c) nodes_lst (leaves_acc, pi_sets_acc) -> 
                    let new_leaves, piset = 
                        List.fold_left (fun (l_acc, p_acc) (n, w) -> 
                            if GameTree.is_leaf n then 
                                (Leaf (n,w,g))::l_acc, p_acc 
                            else l_acc, (n,w)::p_acc) 
                            (leaves_acc, []) nodes_lst in 
                    new_leaves, 
                    match piset with 
                    | [] -> pi_sets_acc 
                    | _ -> (PI_Set (piset, g)::pi_sets_acc)
            ) hash_table ([], [])
    
    let epsilon = 0.05 

    let softmax (alt_scores : (alternative * float) list) (lambda : float): 
            (alternative * float) list = 
        let max_score = 
            List.fold_left 
                (fun best_score (_, score) -> max score best_score) 
                neg_infinity alt_scores in 
        let total = List.fold_left 
            (fun acc (_, score) -> exp (lambda *. (score -. max_score)) +. acc) 
            0. alt_scores in
        List.map (fun (alt, score) -> alt, (exp (lambda *. (score -. max_score)) /. total)) 
                alt_scores

    let rec value (p : player) (avg_opp_str : strategy) (x_set : set) 
            (depth : int) 
            (h : (game * int, (alternative * float) list) Hashtbl.t) 
            (max_depth : int) (lambda : float): float = 
        match x_set with 
        | PI_Set (x_pi_set,g) -> 
            let code = Encoding.encode (GameTree.convert_to_info_set x_pi_set) 
            in
            
            if depth >= max_depth then 
                0. (* puisque que l est un ensemble d'information partielle, 
            ça ne peut pas être une feuille donc inutile d'appeler payoff *)
            else begin
            
            let children = GameTree.opg_children avg_opp_str x_pi_set g in 
            let alt_scores = List.map (fun (y_set, alt) -> alt, value p avg_opp_str 
                y_set (depth+2) h max_depth lambda) children in
            Hashtbl.add h (g, code) (softmax alt_scores lambda);
            List.fold_left 
                (fun best_score (alt, score) -> max best_score score) 
                neg_infinity alt_scores
            end
        | Set l -> 
            let leaves, pi_sets = partition x_set in 

            List.fold_left (fun sum sset -> 
                match sset with 
                | PI_Set _| Set _ -> 
                    failwith "partition ne fonctionne pas correctement."
                | Leaf (leaf, w, g) ->
                    (* tant pis : on a la réponse avec un coup de plus, la partie s'arrête ici
                    donc il ne faut pas la prendre en compte. *)
                    if (leaf.p = p && depth > max_depth) then sum else
                    let score = w *. (GameTree.payoff p leaf)  in 
                    sum +. score
                    ) 0. leaves 
            +.
            List.fold_left (fun sum x -> 
                let score = value p avg_opp_str x depth h max_depth lambda in 
                sum +. score) 0. pi_sets
        | Leaf (leaf,w, g) ->
            failwith "Cas impossible dans la fonction value."

    let imp_minimax (avg_opp_str : strategy) (p : player) 
            (my_last_str : strategy) (max_depth : int) (lambda : float): 
            float * strategy * (game * int, (alternative * float) list) Hashtbl.t =
        let h = Hashtbl.create (1 lsl 15) in 
        let score = 
            match p with 
            | P1 -> 
                let x = (Set [(GameTree.root, 1., [])]) in 
                value P1 avg_opp_str x 0 h max_depth lambda
            | P2 ->
                let x = Set (List.map 
                    (fun opp_alt -> 
                    GameTree.descend opp_alt GameTree.root, 
                    (List.assoc opp_alt (avg_opp_str [] GameTree.root)),
                    [(0,-1)]
                    ) 
                    (GameTree.alternatives [(GameTree.root, 1.)]) 
                ) in
                value P2 avg_opp_str x 1 h max_depth lambda
        in 
        score, Strategy.create_strategy h my_last_str, h


    let rec tpg_value (p : player) (x_set : set) (depth : int) (h : (game * int, (alternative * float) list) Hashtbl.t) (max_depth : int) (lambda : float) : float = 
        match x_set with 
        | PI_Set (x_pi_set,g) -> 
        begin 
            if depth >= max_depth then 0. else (* puisque que l est un ensemble d'information partielle, ça ne peut pas être une feuille donc inutile d'appeler payoff *)
            

            match GameTree.pi_set_player x_pi_set with 
            | p' when p = p' -> 
                let code = Encoding.encode (GameTree.convert_to_info_set x_pi_set) in 
                (* max {V(extend(Y)) | Y is a child of X } + on met la meilleure alternative dans la stratégie *)
                let children = GameTree.tpg_children x_pi_set g in 
                let alt_probs = List.map (fun (y_set, alt) -> alt, tpg_value p y_set (depth+1) h max_depth lambda) children in
                Hashtbl.add h (g, code) (softmax alt_probs lambda);
                List.fold_left (fun best_score (alt, score) -> max best_score score) neg_infinity alt_probs
                
            | _ ->
                (* min {V(extend(Y)) | Y is a child of X }*)
                let children = GameTree.tpg_children x_pi_set g in 
                let alt_probs = List.map (fun (y_set, alt) -> alt, tpg_value p y_set (depth+1) h max_depth lambda) children in
                List.fold_left (fun best_score (alt, score) -> min best_score score) max_float alt_probs
        end
        | Set l -> 
            let leaves, pi_sets = partition x_set in 

            (* Puisque les deux seuls noeuds de chance sont les deux premiers noeuds où l'on choisit la disposition, p(x) = 0.5*0.5 = 0.25 *)
            List.fold_left (fun sum sset -> 
            match sset with 
            | PI_Set _| Set _ -> failwith "partition est ne fonctionne pas correctement."
            | Leaf (leaf, str, g) ->
                let score = 0.25 *. (GameTree.payoff p leaf)  in 
                sum +. score
                    ) 0. leaves 
            +.
            List.fold_left (fun sum x -> let score = tpg_value p x depth h max_depth lambda in 
                sum +. score) 0. pi_sets
        | Leaf (leaf,str, g) -> 0.25 *. (GameTree.payoff p leaf)

    let pure_tpg_imp_minimax (p : player) (depth : int) (lambda : float): strategy = 
        (* Peut servir de stratégie initiale pour la méthode smooth fictitious 
            play. Cet algorithme n'a aucune raison d'être correct.*)
        let h = Hashtbl.create (1 lsl 15) in 
        fun (g : game) (i : info_set) -> 
            let code = Encoding.encode i in
            match Hashtbl.find_opt h (g, code) with 
            | Some alt -> alt 
            | None -> let _ = tpg_value p (Set [(i, 1., g)]) 0 h (depth - (List.length g / 2)) lambda in 
                Hashtbl.find h (g, code)
    

    let update_average_strategy avg_h new_h t =
        Hashtbl.iter (fun key alt_probs ->
            match Hashtbl.find_opt avg_h key with
            | None -> Hashtbl.add avg_h key alt_probs
            | Some avg_probs ->
                let updated_probs = List.map (fun (alt, p) ->
                    let old_p = try List.assoc alt avg_probs with Not_found -> 0.0 in
                    (alt, ((old_p *. float (t - 1)) +. p) /. float t)
                ) alt_probs in
                Hashtbl.replace avg_h key updated_probs
        ) new_h

    let smooth_fictitious_play (nb_rounds : int) (depth : int) 
                                (lambda : float) : strategy * strategy = 
        Printf.printf "Création des stratégies aléatoires initiales.\n";
        let avg_h_p1 = Hashtbl.create (1 lsl 20) in 
        let avg_h_p2 = Hashtbl.create (1 lsl 20) in
        let last_p1_str = ref (Strategy.create_uniform_strategy ()) in 
        let last_p2_str = ref (Strategy.create_uniform_strategy ()) in

        let avg_p1_str = ref !last_p1_str in
        let avg_p2_str = ref !last_p2_str in

        Printf.printf "Stratégies aléatoires intiales créées.\n";
        for round = 1 to nb_rounds do 
            let score1, new_p1_str, h1 = 
                imp_minimax !avg_p2_str P1 !last_p1_str depth lambda in 
            last_p1_str := new_p1_str;
            update_average_strategy avg_h_p1 h1 round;
            avg_p1_str := Strategy.create_strategy avg_h_p1 (Strategy.create_uniform_strategy ());

            Printf.printf "Tour %d prof %d : P1 répond à P2 avec %f.\n" round 
                            depth score1;
            flush stdout;

            let score2, new_p2_str, h2 = 
                imp_minimax !avg_p1_str P2 !last_p2_str depth lambda in 
            last_p2_str := new_p2_str;
            update_average_strategy avg_h_p2 h2 round;
            avg_p2_str := Strategy.create_strategy avg_h_p2 (Strategy.create_uniform_strategy ());

            Printf.printf "Tour %d prof %d : P2 répond à P1 avec %f.\n" round 
                            depth score2;
            flush stdout;
        done;
        !last_p1_str, !last_p2_str


    let smooth_fictitious_play_with_time (time : float) (depth : int) 
        (lambda : float) : unit = 
        (* 709.0 est la valeur maximale de lambda *)

        let stop_time = Sys.time () +. time in
        Printf.printf "Création des stratégies aléatoires initiales.\n";
        let avg_h_p1 = Hashtbl.create (1 lsl 20) in 
        let avg_h_p2 = Hashtbl.create (1 lsl 20) in
        let last_p1_str = ref (Strategy.create_uniform_strategy ()) in 
        let last_p2_str = ref (Strategy.create_uniform_strategy ()) in

        let avg_p1_str = ref !last_p1_str in
        let avg_p2_str = ref !last_p2_str in

        let score_pure1 = ref 0. in 
        let score_pure2 = ref 0. in 
        let score_mixt1 = ref 0. in
        let score_mixt2 = ref 0. in 

        Printf.printf "Stratégies aléatoires intiales créées.\n";

        let rec loop (round : int) : int = 
            let new_score_pure1, new_p1_str, h1 = imp_minimax !avg_p2_str P1 !last_p1_str depth lambda in 
            score_pure1 := new_score_pure1;
            last_p1_str := new_p1_str;

            score_mixt1 := UserInterface.test_strategies !avg_p1_str !avg_p2_str 200 depth;
            score_mixt2 := -.(!score_mixt1);
            let regret_p1 = !score_pure1 -. !score_mixt1 in
            let regret_p2 = !score_pure2 -. !score_mixt2 in
            let exploitability = regret_p1 +. regret_p2 in 
            Printf.printf "Tour %d prof %d : p1_str_%d, meilleure réponse à p2_str_%d, calculée.\n" round depth round (round - 1);
            Printf.printf "Pour le profil (p1_str_%d, p2_str_%d) : \n" (round - 1) (round - 1);
            Printf.printf "Regret de P1 : %f\n" regret_p1;
            if round > 1 then begin 
                Printf.printf "Regret de P2 : %f\n" regret_p2;
                Printf.printf "Exploitabilité : %f\n" exploitability;
            end else begin 
                Printf.printf "Regret de P2 : Non disponible au tour 1\n";
                Printf.printf "Exploitabilité : Non disponible au tour 1\n";
            end;
            Printf.printf "Temps restant : %fs\n" (stop_time -. Sys.time ());
            Printf.printf "\n";
            flush stdout;

            if (Sys.time () >= stop_time) then
                round - 1
            else begin

            update_average_strategy avg_h_p1 h1 round;
            avg_p1_str := Strategy.create_strategy avg_h_p1 (Strategy.create_uniform_strategy ());


            let new_score_pure2, new_p2_str, h2 = imp_minimax !avg_p1_str P2 !last_p2_str depth lambda in 
            score_pure2 := new_score_pure2;
            last_p2_str := new_p2_str;
            
            score_mixt1 := UserInterface.test_strategies !avg_p1_str !avg_p2_str 200 depth;
            score_mixt2 := -.(!score_mixt1);
            let regret_p1 = !score_pure1 -. !score_mixt1 in
            let regret_p2 = !score_pure2 -. !score_mixt2 in
            let exploitability = regret_p1 +. regret_p2 in 
            Printf.printf "Tour %d prof %d : p2_str_%d, meilleure réponse à p1_str_%d, calculée.\n" round depth round round;
            Printf.printf "Pour le profil (p1_str_%d, p2_str_%d) : \n" round (round - 1);
            Printf.printf "Regret de P1 : %f\n" regret_p1;
            Printf.printf "Regret de P2 : %f\n" regret_p2;
            Printf.printf "Exploitabilité : %f\n" exploitability;
            Printf.printf "Temps restant : %fs\n" (stop_time -. Sys.time ());
            Printf.printf "\n";
            flush stdout;

            update_average_strategy avg_h_p2 h2 round;
            avg_p2_str := Strategy.create_strategy avg_h_p2 (Strategy.create_uniform_strategy ());
            loop (round + 1) 
            end
        in
        let round = loop 1 in
        StrategyIO.save_hashtbl_strategy avg_h_p1 (Printf.sprintf "p1t%dd%dl%.2f" round depth lambda);
        StrategyIO.save_hashtbl_strategy avg_h_p2 (Printf.sprintf "p2t%dd%dl%.2f" (round-1) depth lambda);
end


let () = IMP_MINIMAX.smooth_fictitious_play_with_time 20.0 10 100.0