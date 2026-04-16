type player = P1 | P2                               (* Représente un joueur *)

type node = player * string * int * int * int * int (* joueur, plateau, puis le nombre de fantômes vivants pour respectivement les fantômes 
                                                       bleus, rouges, bleus adverses, rouges adverses *)
                                                    (* Pour le plateau : 
                                                      '.' : case vide
                                                      'B' + i : fantôme bleu de P1 provenant de la i-ème case de départ
                                                      'R' + i : fantôme rouge de P1 provenant de la i-ème case de départ
                                                      'r' + i : fantôme bleu de P2 provenant de la i-ème case de départ
                                                      'b' + i : fantôme rouge de P2 provenant de la i-ème case de départ

                                                      ( " 'B' + i " désigne char_of_int ((int_of_char 'B') + i)  )
                                                    *)

type info_set = player * string * int * int * int * int 
                                                    (* Représente un ensemble d'information *)

type pi_set = node list                             (* pi_set : partial information set. 
                                                        Représente un sous-ensemble d'un ensemble d'information. *)

type alternative = int * int                        (* (k_from, k_to) as alt : alternative. 
                                                        Représente un coup :
                                                        Si k_from >= 0 et k_to >= 0, alors alt représente un coup ordinaire : 
                                                            (k_from, k_to) = (case de départ, case d'arrivée). 
                                                        Si k_from < 0, alors alt représente un coup de début de parti 
                                                            qui choisit une disposition des fantômes :
                                                            k_from = -i avec i > 0 où i-1 est l'indice de la disposition 
                                                            des fantômes choisie dans Placement.init_possible_placements_array
                                                        Si k_from >= 0 et k_to < 0, alors alt représente un coup de début
                                                            de partie où la disposition des fantômes est choisie aléatoirement / est cachée 
                                                            à l'adversaire.
                                                            Dans la suite on utilise généralement (0, -1) pour représenter un tel
                                                            coup. *)

type game = alternative list                        (* la liste des coups joués dans la partie, le dernier coup joué étant en tête de liste.
                                                        Représente une partie. *)

type strategy = game -> info_set -> alternative     (* Pour jouer un coup, il faut connaitre les coups précédents (game) et l'état actuel du jeu (info_set).
                                                        Celui-ci peut cependant se déduire de la partie (game), nous le conservons pour plus d'efficacité.
                                                        Représente une stratégie *)

type ghost = B | R  | EmptyCase                     (* Représente un fantôme. *)

type set =  | Set of (node * game) list             (* Représente un ensemble de noeuds (états de la partie). *)
            | PI_Set of node list * game
            | Leaf of node * game



(*================================================================================
  ================================= Paramètres ===================================
  ================================================================================*)
module Config = struct
    type config = Config3x3 | Config3x4 | Config4x4 | Config6x6
    let current_config = Config3x4

    let configuration_6x6 = (6, 6, 4, 4, [25;26;27;28;31;32;33;34], [1;2;3;4;7;8;9;10], [0; 5], [30;35],  [6; 1; -6; -1])
    let configuration_4x4 = (4, 4, 1, 1, [13;14], [1;2], [0;3], [12;15], [4; 1; -4; -1])
    let configuration_3x4 = (4, 3, 1, 1, [9;10], [1;2], [0;4], [8; 11], [4; 1; -4; -1])
    let configuration_3x3 = (3, 3, 1, 1, [6;7], [0;1], [2], [8], [3; 1; -3; -1])

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

    let () = assert (number_of_blue_ghosts + number_of_red_ghosts = List.length p1_placement_cases)
    let () = assert (number_of_blue_ghosts + number_of_red_ghosts = List.length p2_placement_cases)
end





(*================================================================================
  ================================= Utilitaires ==================================
  ================================================================================*)
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

    let other = function P1 -> P2 | P2 -> P1

    let char (p : player) (g : ghost) (cnt : int) : char = 
        (*  entrée : cnt : case de provenance du fantôme 
            sortie : le caractère du fantôme correspondant aux entrées, selon la documentation du type node.*)
        match p with 
        | P1 -> begin 
            match g with 
            | B -> char_of_int (int_of_char 'B' + cnt)
            | R -> char_of_int (int_of_char 'R' + cnt)
            | EmptyCase -> '.'
        end
        | P2 -> begin 
            match g with 
            | B -> char_of_int (int_of_char 'b' + cnt)
            | R -> char_of_int (int_of_char 'r' + cnt)
            | EmptyCase -> '.'
        end

    let read_char (c : char) : (player option) * ghost * int = 
        (* sortie : le type de fantôme et sa case de provenance *)
        match c with
        | '.' -> None, EmptyCase, -1 (* Le joueur renvoyé pour cette option n'a pas d'importance *)
        | _ when c >= 'r' -> Some P2, R, int_of_char c - int_of_char 'r'
        | _ when c >= 'b' -> Some P2, B, int_of_char c - int_of_char 'b'
        | _ when c >= 'R' -> Some P1, R, int_of_char c - int_of_char 'R'
        | _ when c >= 'B' -> Some P1, B, int_of_char c - int_of_char 'B'
        | _ -> failwith ("Impossible de lire le caractère donné : " ^ (Char.escaped c) ^ ". (Tools.read_char)")

    let () = 
        assert (read_char 'R' = (Some P1, R, 0));
        assert (read_char 'B' = (Some P1, B, 0));
        assert (read_char 'S' = (Some P1, R, 1));
        assert (read_char 'C' = (Some P1, B, 1));
        assert (read_char 'r' = (Some P2, R, 0));
        assert (read_char 'b' = (Some P2, B, 0));
        assert (read_char 's' = (Some P2, R, 1));
        assert (read_char 'c' = (Some P2, B, 1));
        assert (read_char '.' = (None, EmptyCase, -1))

    let current_ghost_fn (p : player) : char -> bool = 
        (* sortie : une fonction char -> bool qui renvoie true si le caractère répresente un fantôme de p, et false sinon *)
        fun c -> let (p_g, _, _) = read_char c in p_g = Some p

    let rec power (n : int) (a : int) = 
        (*  précondition : n >= 0
            sortie : a^n *)
        if n = 0 then 1 else a * (power (n-1) a)

    let power_of_2 = Array.init 1000 (fun i -> power i 2)

    let print_node ((p, b, b1, r1, b2, r2) : info_set) : unit = 
        for i = (Config.board_height-1) downto 0 do
            Printf.printf "|";
            for j = 0 to (Config.board_width-1) do
            Printf.printf " %c " b.[i*Config.board_width+j]
            done;
            Printf.printf "|\n"
        done;
        Printf.printf "C'est à %s de jouer\n" (match p with P1 ->"P1" | P2 -> "P2");
        Printf.printf "Nombre de fantômes bleus de P1 vivants : %d\n" b1;
        Printf.printf "Nombre de fantômes rouges de P1 vivants : %d\n" r1;
        Printf.printf "Nombre de fantômes bleus de P2 vivants : %d\n" b2;
        Printf.printf "Nombre de fantômes rouges de P2 vivants : %d\n" r2
    
    let print_info_set ((p, b, b1, r1, b2, r2) : info_set) : unit = 
        for i = (Config.board_height-1) downto 0 do
            Printf.printf "|";
            for j = 0 to (Config.board_width-1) do
                match read_char b.[i*Config.board_width+j] with 
                | Some opp, _, _ when opp <> p -> Printf.printf " ? "
                | _ -> Printf.printf " %c " b.[i*Config.board_width+j]
            done;
            Printf.printf "|\n"
        done;
        Printf.printf "C'est à %s de jouer\n" (match p with P1 ->"P1" | P2 -> "P2");
        Printf.printf "Nombre de fantômes bleus de P1 vivants : %d\n" b1;
        Printf.printf "Nombre de fantômes rouges de P1 vivants : %d\n" r1;
        Printf.printf "Nombre de fantômes bleus de P2 vivants : %d\n" b2;
        Printf.printf "Nombre de fantômes rouges de P2 vivants : %d\n" r2

    let print_list (l : 'a list) (print_fn : 'a -> unit) : unit = 
        (* affiche la liste donnée en entrée, chacun des éléments étant affiché à l'aide de print_fn.*)
        Printf.printf "[";
        let rec aux (l : 'a list) : unit = 
            match l with 
            | [] -> Printf.printf "]"
            | [elt] -> print_fn elt; Printf.printf "]"
            | elt::l' -> print_fn elt; Printf.printf ";"; aux l'
        in 
        aux l

    let shuffle_list (l : 'a list) : 'a list=
        (* sortie : la liste mélangée en O(n) grâce à l'algorithme de Fisher-Yates *)
        let arr = Array.of_list l in
        let n = Array.length arr in
        for i = n - 1 downto 1 do
            let j = Random.int (i + 1) in
            let tmp = arr.(i) in
            arr.(i) <- arr.(j);
            arr.(j) <- tmp;
        done;
        Array.to_list arr

    let print_games (games : alternative list list) = 
        print_list games (fun game -> print_list game (fun (k_from, k_to)-> Printf.printf "(%d, %d)" k_from k_to); print_newline ())
    

    let compare_scores (score1 : float) (score2 : float) (epsilon : float) : bool = 
        (* sortie : |score1 - score2| <= epsilon *)
        abs_float (score1 -. score2) <= epsilon
end      





(*===========================================================================================================
  ====================== Fonctions et algorithmes sur les dispositions des fantômes =========================
  ===========================================================================================================*)
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
            | _, _ -> if Random.bool () then
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

    let add_init_placement ((p, b, b1, r1, b2, r2) : node) (pl : placement) : node = 
        match p with 
        | P1 -> 
            P2,
            (let cnt = ref (-1) in
            String.init 
                Config.board_length 
                (fun k -> 
                    if List.mem k Config.p1_placement_cases then (
                        incr cnt;
                        Tools.char p (pl !cnt) !cnt
                    ) else b.[k] )
            ),
            Config.number_of_blue_ghosts, 
            Config.number_of_red_ghosts, 
            b2,
            r2
        | P2 -> 
            P1,
            (let cnt = ref (-1) in
            String.init 
                Config.board_length 
                (fun k -> 
                    if List.mem k Config.p2_placement_cases then (
                        incr cnt;
                        Tools.char p (pl !cnt) !cnt
                    ) else b.[k] )
            ),
            b1, 
            r1, 
            Config.number_of_blue_ghosts,
            Config.number_of_red_ghosts

    let put_placement ((p, b, b1, r1, b2, r2) : info_set) (pl : placement) : node = 
        (* sortie : un noeud de l'ensemble d'information donné où les fantômes inconnus sont colorés selon pl 
        Remarque : ne fonctionne pas correctement s'il y a un fantôme inconnu de mort. *)
        p,
        (
        String.init 
            Config.board_length
            (fun k -> 
                match Tools.read_char b.[k] with
                | Some opp, _, cnt when opp <> p -> Tools.char (Tools.other p) (pl cnt) cnt (* C'est les fantômes adverses qu'on colorie *)
                | _ -> b.[k]  )
        ),
        b1, 
        r1, 
        b2, 
        r2

    let is_placement_node (p, b, b1, r1, b2, r2 : node) : bool = 
        (* Sortie : true ssi le noeud donné argument est la racine ou un fils de la racine, 
            i.e, un noeud où les alternatives sont des placements et non des mouvements de fantôme.*)
        (b1 = 0 && r1 = 0) || (b2 = 0 && r2 = 0)


    let print_placement (n : node) (pl : placement) : unit = 
        let (p, b, b1, r1, b2, r2) = add_init_placement n pl in (* on ajoute le placement mais il pense 
                                                                            donc que c'est au joueur suivant de jouer *)
        for i = (Config.board_height-1) downto 0 do
            Printf.printf "|";
            for j = 0 to (Config.board_width-1) do
                match Tools.read_char b.[i*Config.board_width+j] with 
                | Some p', _, _ when p' = p -> Printf.printf " ? " (* c'est donc bien les fantômes adverses *)
                | _ -> Printf.printf " %c " b.[i*Config.board_width+j]
            done;
            Printf.printf "|\n"
        done;


end





module GameTree = struct 

    let descend (k_from, k_to : alternative) ((p, b, b1, r1, b2, r2) as n : node): node = 
        (* sortie : renvoie le noeud dérivant du noeud n en jouant a *)
        if k_from < 0 then 
            Placement.add_init_placement n Placement.init_possible_placements_array.(-k_from-1)
        else if k_to < 0 then 
            Placement.add_init_placement n Placement.init_possible_placements_array.(Random.int Placement.number_of_init_possible_placements)
        else 
            let b1', r1', b2', r2' = 
                match Tools.read_char b.[k_to] with 
                | None, EmptyCase, -1 -> b1, r1, b2, r2
                | Some P1, B, _ -> b1-1,r1, b2, r2
                | Some P1, R, _ -> b1, r1-1, b2, r2
                | Some P2, B, _ -> b1, r1, b2-1, r2
                | Some P2, R, _ -> b1, r1, b2, r2-1
                | _ -> failwith "Ce cas est impossible."
            in
            Tools.other p, 
            String.init 
                Config.board_length 
                (fun k -> if k = k_from then '.' else if k = k_to then b.[k_from] else b.[k]),
            b1',
            r1',
            b2', 
            r2'

    (* Premiers noeuds de l'arbre en Config4x4 *)
    let root = P1, String.make Config.board_length '.', 0, 0, 0, 0
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

    let alternatives (x_set : pi_set) : alternative list = 
        (* sortie : la liste des coups possibles depuis un ensemble d'information partielle.*)

        let (p, b, b1, r1, b2, r2)= List.hd x_set in (* Un seul noeud du PI-Set suffit 
                                                        pour déterminer les coups jouables puisque chacun 
                                                        des noeuds ont les mêmes coups possibles étant donné
                                                        qu'ils appartiennent au même PI-Set *)
        if b1 + b2 + r1 + r2 = 3 then []        (* partie terminée -> pas de coups possibles *)
        else if b1 + b2 + r1 + r2 <= 2 then (   (* début de partie -> les coups possibles sont des dispositions de fantômes.*)
            assert ((b1 = 0 && r1 = 0) || (b2 = 0 && r2 = 0));
            List.init (Placement.number_of_init_possible_placements) (fun i -> (-i-1, 0))
        ) else

        let is_current_ghost = Tools.current_ghost_fn p in

        let rec explore (k_from : int) (acc : alternative list) : alternative list = 
            (*  parcourt récursivement toutes les cases du plateau de manière décroissante 
                pour déterminer tous les coups possibles. *)
            if k_from = -1 then acc else 
            explore
                (k_from-1) 
                (if not (is_current_ghost b.[k_from]) then 
                    acc 
                else
                    List.fold_left (fun acc move -> 
                        let k_to = k_from + move in
                        if on_the_board k_from k_to && not (is_current_ghost b.[k_to]) then (k_from, k_to)::acc
                        else acc
                    ) acc Config.possible_moves  ) in

        explore (Config.board_length-1) []

    let convert_to_info_set (x_set : pi_set) : info_set = 
        (* précondition : x_set est non vide et x_set est un ensemble d'informations partiel pour le joueur dont c'est le tour *)
        (* Un info set est simplement un noeud où les fantômes adverses ne sont pas forcément de la couleur renseignée *)
        List.hd x_set

    let convert_to_pi_set ((p, _, b1, r1, b2, r2) as i : info_set) : pi_set = 
        (* sortie : renvoie la liste des noeuds que contient l'ensemble d'information représenté par i 
        Remarque : en configuration 4x4, il ne peut y avoir deux fois le même noeud dans le pi_set renvoyé puisque le jeu s'arrête une fois qu'il y a
        moins de 4 fantômes sur le plateau, mais en configuration supérieure il peut y avoir énormémement de redondance. Mais celles-ci rendent compte 
        de la probabilité d'avoir tel ou tel fantôme à tel endroit si l'on considère que la disposition des fantômes initiale est équiprobable. 
        Remarque 2 : cette fonction ne fonctionne pas correctement si un fantôme inconnu est mort (donc pas de problème en Configuration4x4, à part pour 
        les états finaux) puisque Placement.put_placement ne fonctionne pas correctement si un fantôme inconnu est mort *)
        List.map 
            (fun pl -> Placement.put_placement i pl) 
            (Placement.init_possible_placements)

    let pi_set_player (x_pi_set : pi_set) : player = 
        (* sortie : le joueur dont c'est le tour *)
        match x_pi_set with 
        | [] -> failwith "Impossible de déterminer le joueur d'un ensemble d'information partielle vide."
        | (p, _, _, _, _, _)::_ -> p



    let is_leaf ((p, b, b1, r1, b2, r2) : node) : bool = 
        (*  entrée : un noeud
            sortie : true ssi le noeud est une feuille, i.e la partie est terminée dans ce noeud.  *)

        List.exists (fun k -> match Tools.read_char b.[k] with Some P1, B, _ -> true | _ -> false) Config.p1_exit_cases || 
        List.exists (fun k -> match Tools.read_char b.[k] with Some P2, B, _ -> true | _ -> false) Config.p2_exit_cases || 
        (b1 = 0 && r1 <> 0) || 
        (r1 = 0 && b1 <> 0) || 
        (b2 = 0 && r2 <> 0) || 
        (r2 = 0 && b2 <> 0) (* && ... pour prendre en compte la racine et ses enfants (= les noeuds de placement)*)


    let payoff (p : player) ((_, b, b1, r1, b2, r2) : node) : float = 
        (* précondition : le noeud est une feuille 
        sortie : -1 si le joueur donné perd, 1 s'il gagne, 0 sinon*)

        
        (* On désambiguise une règle : si un fantôme bleu sort en mangeant un fantôme rouge, le fantôme bleu gagne. *)
        if List.exists (fun k -> match Tools.read_char b.[k] with Some P1, B, _ -> true | _ -> false) Config.p1_exit_cases  then 
            if p = P1 then 1. else -1.
        else if List.exists (fun k -> match Tools.read_char b.[k] with Some P2, B, _ -> true | _ -> false) Config.p2_exit_cases then
            if p = P1 then -1. else 1.
        else if b2 = 0 || r1 = 0 then 
            if p = P1 then 1. else -1.
        else if b1 = 0 || r2 = 0 then
            if p = P1 then -1. else 1.
        else
            0.



    let opg_children (pi : strategy) (x_set : pi_set) (g : game) : (set * alternative) list = 
        (* One player game children 
        pré-condition : x_set est non vide, ne contient pas deux noeuds identiques
        sortie : la liste des enfants de l'ensemble x_set. Un enfant est un ensemble d'ensembles à informations imparfaites
            directement en dessous de x_set dans l'arbre à un joueur via une coup précis.*)

        (* Etape 1 : on génère les pi_sets découlant des alternatives possibles pour le joueur à qui c'est le tour *)
        let pi_sets_games_alts = List.map (fun alt -> ((List.map (descend alt) x_set),alt::g, alt)) (alternatives x_set) in 

        (* Etape 2 : on transforme ces pi_sets en information imparfaite pour le joueur adverse *)
        (* Rien à faire : chacun des noeuds d'un pi_set de pi_sets_alts représente déjà un ensemble d'information différent 
        pour le joueur adverse
        En effet, soit un ensemble d'information partiel x de pi_sets_alts. On sait qu'il n'y pas deux noeuds identiques dans x (précondition)
        Donc toutes les noeuds représentent des dispositions de fantômes de l'adversaire différentes. Donc les noeuds de x représentent 
        chacun un ensemble d'information différent pour l'adversaire. *)

        (* Etape 3 : on récupère chacun des ensembles (qui ne sont plus des ensembles d'information) 
        découlant de l'alternative choisie par le joueur adverse sous la stratégie pi *)
        List.fold_left (fun acc (pi_set, g, alt) -> 
                    (Set (List.map 
                        (fun n -> 
                            if is_leaf n then 
                                n, g
                            else
                                let opp_alt = pi g (convert_to_info_set [n]) in 
                                descend opp_alt n, opp_alt::g ) 
                        pi_set
                        ), alt)::acc
                ) [] pi_sets_games_alts

    let tpg_children (x_set : pi_set) (g : game) : (set * alternative) list = 
        (* Two player game children 
        pré-condition : x_set est non vide, ne contient pas deux noeuds identiques
        post-condition : la liste des enfants de l'ensemble x_set. Un enfant est un ensemble d'ensembles à informations imparfaites
            directement en dessous de x_set dans l'arbre à deux joueurs via une coup précis.*)

        (* Etape 1 : on génère les ensembles de noeuds découlant des alternatives possibles pour le joueur à qui c'est le tour *)
        List.map (fun alt -> (Set (List.map (fun n -> descend alt n, alt::g) x_set), alt)) (alternatives x_set)

end




module Encoding = struct 
    let encode ((p, b, b1, r1, b2, r2) as n : info_set) : int = 
        (* précondition : on est en config4x4
        sortie : on code sur 22 bits l'ensemble d'information qui représente un état du jeu : 
            - 4 premiers bits = position (indice k) fantôme bleu connu
            - 4 bits suivants = position fantôme rouge connu 
            - 4 bits suivant : position du fantôme inconnu provenant de la case de départ n°0
            - 4 bits suivant : position du fantôme inconnu provenant de la case de départ n°1
            - bit suivant : b1
            - bit suivant : r1
            - bit suivant : b2
            - bit suivant : r2
            - bit de fin : p (0 si p = P1 et 1 si p = P2)
        (dans le sens inverse) *)
        
        let b_pos = ref 0 in (* k pour known *)
        let r_pos = ref 0 in 
        let u0_pos = ref 0 in (* u pour unknown *)
        let u1_pos = ref 0 in 
        let blue_num = ref 0 in (* num de la case de provenance du fantôme bleu connu *)
        let unknown_origin = ref (-1) in (* Variable utile seulement s'il n'y a qu'un seul fantôme inconnu de vivant. 0 s'il provient de 0, 1 sinon *)
        for k = 0 to Config.board_length-1 do 
            match Tools.read_char b.[k] with 
            | None, _, _ -> () (* case vide*)
            | Some p_g, B , 0 when p_g = p -> b_pos := k; blue_num := 0
            | Some p_g, R , 0 when p_g = p -> r_pos := k; blue_num := 1
            | Some p_g, B, 1 when p_g = p -> b_pos := k; blue_num := 1;
            | Some p_g, R, 1 when p_g = p -> r_pos := k; blue_num := 0;
            | Some opp, _ , 0 when opp <> p -> u0_pos:= k; unknown_origin := 0
            | Some opp, _ , 1 when opp <> p -> u1_pos:= k; unknown_origin := 1
            | _ -> failwith "Cas impossible.\n"
        done;
        (* S'il n'y a qu'un seul fantôme inconnu de vivant, il faut préciser si c'est u0 ou u1, pour cela on modifie u_pos du fantôme mort 
        de manière à ce que u0_pos + u1_pos donne un nombre pair si c'est le fantôme u0 qui est toujours vivant et impair si c'est u1 *)
        if (match p with P1 -> b2 + r2 = 1 | P2 -> b1 + r1 = 1) then 
            if !unknown_origin = 0 then u1_pos := !u0_pos mod 2             (* u0_pos + u1_pos = pair *)
            else if !unknown_origin = 1 then u0_pos := 1 + (!u1_pos mod 2)   (* u0_pos + u1_pos = impair *)
            else begin 
                Printf.printf "Attention : voici le noeud qui provoque une erreur :\n";
                Tools.print_node n;
                failwith "Erreur lors de l'encodage : le seul fantôme inconnu de vivant n'a pas été repéré.\n" end;
        !b_pos + 
        (Tools.power_of_2.(4)* !r_pos) + 
        (Tools.power_of_2.(8) * !u0_pos) +
        (Tools.power_of_2.(12) * !u1_pos) +
        (Tools.power_of_2.(16) * b1) +
        (Tools.power_of_2.(17) * r1) +
        (Tools.power_of_2.(18) * b2) +
        (Tools.power_of_2.(19) * r2) +
        (Tools.power_of_2.(20) * (match p with | P1 -> 0 | P2 -> 1)  ) +
        (Tools.power_of_2.(21) * !blue_num)

    let decode (code : int) : info_set option = 
        (* précondition : code >= 0 et on est config4x4 et il existe un unique info_set n tel que encode n = code.
        sortie : n *)
        let code, b_pos = code / 16, code mod 16 in 
        let code, r_pos = code / 16, code mod 16 in 
        let code, u0_pos = code / 16, code mod 16 in 
        let code, u1_pos = code / 16, code mod 16 in 
        let code, b1 = code / 2, code mod 2 in 
        let code, r1 = code / 2, code mod 2 in 
        let code, b2 = code / 2, code mod 2 in 
        let code, r2 = code / 2, code mod 2 in 
        let code, p = code / 2, code mod 2 in 
        let code, blue_num = code / 2, code mod 2 in
        if  code = 0 
            && (
            (if b1 + r1 + b2 + r2 = 4 then 
                (b_pos <> r_pos &&
                b_pos <> u0_pos &&
                b_pos <> u1_pos &&
                r_pos <> u0_pos &&
                r_pos <> u1_pos &&
                u0_pos <> u1_pos) else false
            ) ||
            (if b1 + r1 + b2 + r2 = 3 then 
                true (* A faire *)
                else false
            ) ||
            (if b1 = 0 && r1 = 0 then 
                b2 = 0 && r2 = 0 &&
                b_pos = 0 &&
                r_pos = 0 &&
                u0_pos = 0 && 
                u1_pos = 0 else false
            ) ||
            (if b2 = 0 && r2 = 0 then 
                (b1 = 0 && r1 = 0) ||
                true (* A faire*) else false)
            ) then 
        Some (
        match p with
        | 0 ->
            P1,
            (* Plusieurs possibilités pour le joueur 2 inconnu : soit il a deux fantômes, soit il n'en a qu'un seul (ou 0 mais ça ne change rien) 
                auquel cas il faut déterminer si c'est u0 ou u1 *)
            (if b2 + r2 = 2 then 
                String.init Config.board_length (fun k ->  
                    if k = b_pos && b1 = 1 then Tools.char P1 B blue_num else 
                    if k = r_pos  && r1 = 1 then Tools.char P1 R (1-blue_num) else 
                    if k = u0_pos then Tools.char P2 B 0 else       (* les couleurs des fantômes adverses renseignées n'ont *)
                    if k = u1_pos then Tools.char P2 R 1            (* aucune importance puisqu'il s'agit d'un info_set     *)
                    else '.')
            else if b2 + r2  = 0 then
                String.init Config.board_length (fun k ->  
                    if k = b_pos && b1 = 1 then Tools.char P1 B blue_num else 
                    if k = r_pos  && r1 = 1 then Tools.char P1 R (1-blue_num)
                    else '.')
            else if (u0_pos + u1_pos) mod 2 = 0 then (* Alors c'est le fantôme u0 qui est toujours vivant. *)
                String.init Config.board_length (fun k ->  
                    if k = b_pos && b1 = 1 then Tools.char P1 B blue_num else 
                    if k = r_pos  && r1 = 1 then Tools.char P1 R (1-blue_num) else  
                    if k = u0_pos then Tools.char P2 B 0
                    else '.')
            else (* Alors c'est le fantôme u0 qui est toujours vivant *)
                String.init Config.board_length (fun k ->  
                    if k = b_pos && b1 = 1 then Tools.char P1 B blue_num else 
                    if k = r_pos  && r1 = 1 then Tools.char P1 R (1-blue_num) else  
                    if k = u1_pos then Tools.char P2 R 1
                    else '.')
            ),
            b1,
            r1,
            b2,
            r2 
        | 1 -> 
            P2,
            (if b1 + r1 = 2 then 
                String.init Config.board_length (fun k ->  
                    if k = b_pos && b2 = 1 then Tools.char P2 B blue_num else 
                    if k = r_pos && r2 = 1 then Tools.char P2 R (1-blue_num) else
                    if k = u0_pos then Tools.char P1 B 0 else       (* les couleurs des fantômes adverses renseignées n'ont *)
                    if k = u1_pos then Tools.char P1 R 1            (* aucune importance puisqu'il s'agit d'un info_set     *)
                    else '.')
            else if b1 + r1  = 0 then
                String.init Config.board_length (fun k ->  
                    if k = b_pos && b2 = 1 then Tools.char P2 B blue_num else 
                    if k = r_pos && r2 = 1 then Tools.char P2 R (1-blue_num)
                    else '.')
            else if (u0_pos + u1_pos) mod 2 = 0 then (* Alors c'est le fantôme u0 qui est toujours vivant. *)
                String.init Config.board_length (fun k ->  
                    if k = b_pos && b2 = 1 then Tools.char P2 B blue_num else 
                    if k = r_pos && r2 = 1 then Tools.char P2 R (1-blue_num) else
                    if k = u0_pos then Tools.char P1 B 0
                    else '.')
            else (* Alors c'est le fantôme u0 qui est toujours vivant *)
                String.init Config.board_length (fun k ->  
                    if k = b_pos && b2 = 1 then Tools.char P2 B blue_num else 
                    if k = r_pos && r2 = 1 then Tools.char P2 R (1-blue_num) else 
                    if k = u1_pos then Tools.char P1 R 1
                    else '.')
            ),
            b1,
            r1,
            b2, 
            r2 
        | _ -> failwith "Impossible de décoder : code négatif.") else None

    let compare_code ((p, b, b1, r1, b2, r2) : node) (decoded_n: node option) : bool = 
        (* sortie : true ssi decoded_n correspond à (p, b, b1, r1, b2, r2) si l'on ne considère que les fantômes connus.
        Cette fonction sert à vérifier que code et decode fonctionnent correctement. *)
        match decoded_n with 
        | None -> false 
        | Some (p', b', b1', r1', b2', r2') -> begin
            p = p' && b1 = b1' && r1 = r1' && b2 = b2' && r2 = r2' 
            && List.for_all (fun k -> 
                match Tools.read_char (b'.[k]) with 
                | Some opp, B, 0 when opp <> p -> true
                | Some opp, R, 1 when opp <> p -> true 
                | _ -> b.[k] = b'.[k]) (List.init Config.board_length (fun k -> k))
        end

    let rec test_code (n : node) (cnt : int) (max_cnt : int) (history : alternative list): unit = 
        (* test récursvivement les fonctions code et décodent sur tous les descendants de n jusqu'à la profondeur 
        max_cnt - cnt. *)
        if not (compare_code n (decode (encode n))) then (
            Printf.printf "L'encodage perd de l'information pour le noeud suivant : \n";
            Printf.printf "n : \n";
            Tools.print_node n;
            Printf.printf "encode n : %d \n" (encode n);

            let decoded = decode (encode n) in
            match decoded with 
            | None -> Printf.printf "decode (encode n) = None."
            | Some invented_n ->begin 
                Printf.printf "noeud inventé = decode (encode n) : \n";
                Tools.print_node (invented_n);
                Printf.printf "encode (noeud inventé) : %d \n" (encode (invented_n));
                match decode (encode invented_n) with 
                | None -> Printf.printf "encode (decode (encode noeud_inventé)) : None \n"
                | Some invented_invented_n -> begin 
                    Printf.printf "encode (decode (encode noeud_inventé)) : %d \n" (encode invented_invented_n);
                    Printf.printf "history : ";
                    Tools.print_list history (fun (k_from, k_to) -> Printf.printf "(%d, %d)" k_from k_to);
                    print_newline ();
                    assert (encode (invented_invented_n) = encode (invented_n)) end 
                end;
            print_newline();
        );
        if cnt < max_cnt then (
            let alts = GameTree.alternatives [n] in 
            List.iter (fun alt -> test_code (GameTree.descend alt n) (cnt+1) max_cnt (alt::history)) alts
        )

end 





module Strategy = struct 
    let random_mixt_alt (alts : alternative list) (g: game) (i : info_set) (seed : int) : alternative = 
        (* sortie : un élt de alts choisi par une fonction inconnue (Hashtbl.hash ici) fonction de g et seed. 
        Pour alts, g et seed fixés, random_alt alts g seed donne donc toujours le même résultat. *)
        if fst (List.hd alts) < 0 then (0, -1) else
        let n = List.length alts in 
        let random_index = Hashtbl.hash (g, i, seed) mod n in 
        List.nth alts random_index

    let create_random_mixt_strategy_2 () : strategy = 
        let seed = Random.int 1000000000 in 
        fun (g : game) (i : info_set) -> 
            match Encoding.decode (Encoding.encode i) with 
            | None -> failwith "L'info set donné en argument à la random strategy n'est pas valide."
            | Some i ->
                random_mixt_alt (GameTree.alternatives [i]) g i seed

    let create_strategy (h : ((game * int), alternative) Hashtbl.t) (last_str : strategy) : strategy = 
        (* sortie : créer une stratégie qui renvoie le coup de h si l'état (g,i) est 'dans' h et 
            renvoie le coup de last_str sinon *)
        fun (g : game) (i : info_set) -> 
        match Hashtbl.find_opt h (g, Encoding.encode i) with 
        | None -> last_str g i
        | Some alt -> alt
  
    let create_random_strategy () : strategy * alternative array = 
        let random_array = 
            Array.init 
                Tools.power_of_2.(22) 
                (fun code -> 
                    let decoded = Encoding.decode code in 
                    match decoded with 
                    | None -> (-42, -42)
                    | Some i ->
                        let pi_set = GameTree.convert_to_pi_set i in 
                        let alts = GameTree.alternatives pi_set in
                        if alts = [] then (-43, -43)
                        else
                        List.nth alts (Random.int (List.length alts)) 
                )
        in (fun (g : game) (i : info_set) -> random_array.(Encoding.encode i)), random_array
    
    let create_random_mixt_strategy () : strategy = 
        let random_array = 
            Array.init 
                Tools.power_of_2.(22) 
                (fun code -> 
                    let decoded = Encoding.decode code in 
                    match decoded with 
                    | None -> (-42, -42)
                    | Some i ->
                        let pi_set = GameTree.convert_to_pi_set i in 
                        let alts = GameTree.alternatives pi_set in
                        if alts = [] then (-43, -43) 
                        else if fst (List.hd alts) < 0 then (0,-1) (* Si c'est une disposition alors on dit à la fonction descend de choisir au hazard : (0,-1)*)
                        else
                        List.nth alts (Random.int (List.length alts)) 
                )
        in (fun (g : game)  (i : info_set) -> random_array.(Encoding.encode i))
end 






module UserInterface = struct 

    let rec user_alternative ((p, b, _, _, _, _) as n : node) : alternative = 
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
                if GameTree.on_the_board k_from k_to && (Tools.current_ghost_fn p b.[k_from] && not(Tools.current_ghost_fn p b.[k_to])) then
                    (k_from, k_to)
                else
                    failwith "Les règles n'autorisent pas ce coup.\n"
            end
        with Failure message ->
            print_string message;
            user_alternative n

    let user_against_pure_strategy (pi : strategy) (user_player : player) : unit = 
        (* L'utilisateur joue avec le joueur user_player contre la stratégie pure pi *)
        let rec play_game ((p, b, b1, r1, b2, r2) as n : node) (g : game): unit = 
            if GameTree.is_leaf n then (
                Tools.print_node n;
                match p, GameTree.payoff p n with 
                | P1, 1. | P2, -1. -> Printf.printf "Le joueur P1 gagne la partie !\n"
                | P1, -1. | P2, 1. -> Printf.printf "Le joueur P2 gagne la partie !\n"
                | _ -> Printf.printf "Il y a égalité... Ce n'est pas possible dans ce jeu donc il y a une erreur dans le programme !\n"
            ) else 
                let alt = 
                    if p = user_player then 
                        user_alternative n
                    else 
                        pi g (GameTree.convert_to_info_set [n])
                in play_game (GameTree.descend alt n) (alt::g) in 
        play_game GameTree.root []
    
    exception IncorrectAnswer 

    let rec user_against_mixt_strategy (pi : strategy) (user_player : player) : unit = 
        (* L'utilisateur joue avec le joueur user_player contre la stratégie mixte pi *)
        try
        Printf.printf "Voulez-voir la couleur des fantômes adverses ? ('1' : oui, '0': non) \n";
        let visible = 
            try read_int () == 1 with Failure _ -> raise IncorrectAnswer in
        let rec play_game ((p, b, b1, r1, b2, r2) as n : node) (g : game): unit = 
            if GameTree.is_leaf n then (
                Tools.print_node n;
                match p, GameTree.payoff p n with 
                | P1, 1. | P2, -1. -> Printf.printf "Le joueur P1 gagne la partie !\n"
                | P1, -1. | P2, 1. -> Printf.printf "Le joueur P2 gagne la partie !\n"
                | _ -> Printf.printf "Il y a égalité... Ce n'est pas possible dans ce jeu donc il y a une erreur dans le programme !\n"
            ) else 
                let alt = 
                    if p = user_player then (
                        if visible then 
                            Tools.print_node (GameTree.convert_to_info_set [n])
                        else 
                            Tools.print_info_set (GameTree.convert_to_info_set [n]);
                        user_alternative n
                    ) else 
                        pi g (GameTree.convert_to_info_set [n]) in 
                let g' = if List.length g < 2 then (0, -1)::g else alt::g in (* Ici est la différence entre user_against_pure_strategy
                                                                                et user_against_mixt_strategy : pour mixt strategy il
                                                                                ne faut pas dévoiler la disposition des fantômes, c'est
                                                                                pourquoi l'on met (0, -1) dans les coups joués au début. *)
                play_game (GameTree.descend alt n) g' in 
        play_game GameTree.root []

        with IncorrectAnswer -> 
            Printf.printf "Veuillez renseigner '0' ou '1'.\n"; 
            user_against_mixt_strategy pi user_player

    let strategy_against_strategy (p1_str : strategy) (p2_str : strategy) (max_cnt : int): player option = 
        (* Deux stratégies (pures ou mixtes) s'affrontent avec un nombre de coups (total, pas chacun) limité à max_cnt.
        Sortie : Si la partie dépasse max_cnt coups, il y a égalité : None est renvoyée. Sinon Some (le joueur gagnant) 
            est renvoyée *)
        let rec play_game ((p, b, b1, r1, b2, r2) as n : node) (cnt : int) (g: game): player option= 
            if GameTree.is_leaf n || cnt > max_cnt then (
                match p, GameTree.payoff p n with 
                | P1, 1. | P2, -1. -> Some P1
                | P1, -1. | P2, 1. -> Some P2
                | _ -> None
            ) else 
                let alt = match p with P1 -> p1_str g (GameTree.convert_to_info_set [n]) | P2 -> p2_str g (GameTree.convert_to_info_set [n])
                in play_game (GameTree.descend alt n) (cnt+1) (alt::g) in 
        play_game GameTree.root 0 []

    let show_strategy_against_strategy (p1_str : strategy) (p2_str : strategy) (max_cnt : int) : player option = 
        (* C'est exactement la même fonction que strategy_against_strategy, mais celle-ci affiche l'état du jeu entre 
            chaque coup. *)
        let rec play_game ((p, b, b1, r1, b2, r2) as n : node) (cnt : int) (g : game): player option= 
            Printf.printf "%s\n" b;
            Tools.print_node n;
            if GameTree.is_leaf n || cnt > max_cnt then (
                match p, GameTree.payoff p n with 
                | P1, 1. | P2, -1. -> Some P1
                | P1, -1. | P2, 1. -> Some P2
                | _ -> None
            ) else 
                let alt = match p with P1 -> p1_str g (GameTree.convert_to_info_set [n]) | P2 -> p2_str g (GameTree.convert_to_info_set [n])
                in play_game (GameTree.descend alt n) (cnt+1) (alt::g) in 
        play_game GameTree.root 0 []
    
    let test_strategies (p1_str : strategy) (p2_str: strategy) (max_cnt : int) : float * float = 
        (*  Fait s'affronter les stratégies p1_str et p2_str max_cnt fois
            Sortie : (taux de victoire de P1, taux de victoire de P2) *)
        let nb_of_tests = 1000 in
        let p1_wins = ref 0 in 
        let p2_wins = ref 0 in
        let draws = ref 0 in 
        for cnt = 1 to nb_of_tests do 
            match strategy_against_strategy p1_str p2_str max_cnt with  
            | None -> incr draws
            | Some P1 -> incr p1_wins
            | Some P2 -> incr p2_wins
        done;
        let score1 =  ((float !p1_wins) /. (float nb_of_tests)) in 
        let score2 = ((float !p2_wins) /. (float nb_of_tests)) in 
        let scoreDraw = ((float !draws) /. (float nb_of_tests)) in
        Printf.printf " Realité : P1 : %f, P2 : %f, nulle : %f.\n" score1 score2 scoreDraw;
        score1, score2
end





module IMP_MINIMAX = struct

    let partition (x_set : set) : set list * set list = 
        (*  entrée : x un ensemble de noeuds 
            sortie : un ensemble de pi_set, un ensemble de feuilles. L'union des pi_sets et de l'ensemble des feuilles 
                vaut l'ensemble x_set. Les pi_sets appartiennent à des ensembles d'information deux à deux disjoints. *)
        
        match x_set with 
        | PI_Set _    -> [], [x_set]
        | Leaf _      -> [x_set], []
        | Set l       -> 
            let hash_table = Hashtbl.create Config.number_of_moves in 
            List.iter (fun (n, g) -> 
                let c = Encoding.encode n in 
                match Hashtbl.find_opt hash_table (g,c) with 
                | None -> Hashtbl.add hash_table (g,c) [n]
                | Some (nodes_lst) -> Hashtbl.replace hash_table (g,c) (n::nodes_lst)
            ) l;
            Hashtbl.fold (fun (g,c) nodes_lst (leaves, pi_sets) -> 
                    let new_leaves, piset = 
                        List.fold_left (fun (leaves, piset) n -> if GameTree.is_leaf n then (Leaf (n,g))::leaves, piset else leaves,n::piset) 
                                        ([], []) nodes_lst
                    in new_leaves@leaves, match piset with [] -> pi_sets | _ -> (PI_Set (piset, g)::pi_sets)
            ) hash_table ([], [])
    
    let epsilon = 0.05 

    let rec value (p : player) (pi : strategy) (x_set : set) (depth : int) (h : (game * int, alternative) Hashtbl.t) (max_depth : int) : float = 
        match x_set with 
        | PI_Set (x_pi_set,g) -> 
            let code = Encoding.encode (GameTree.convert_to_info_set x_pi_set) in
            
            if depth >= max_depth then 
                0. (* puisque que l est un ensemble d'information partielle, ça ne peut pas être une feuille donc inutile d'appeler payoff *)
            else begin
            (* max {V(extend(Y)) | Y is a child of X } + on met la meilleure alternative dans la stratégie *)
            let rec max_value (l : (set*alternative) list) (maxi : float)  (maxi_alt : alternative) : float= 
                match l with 
                | [] ->  
                    Hashtbl.add h (g, code) maxi_alt;
                    maxi
                | (y_set, alt) :: l' -> let score = value p pi y_set (depth+1) h max_depth in 
                    if score > (maxi +. epsilon) then max_value l' score alt
                    else if score > (maxi -. epsilon) && Random.bool () then 
                        max_value l' score alt (* maxi et score sont égaux alors on choisit aléatoirement *)
                    else max_value l' maxi maxi_alt in
            
            let chiildren = Tools.shuffle_list (GameTree.opg_children pi x_pi_set g) in 
            max_value (chiildren) (-3.) (-1, -1)
            end
        | Set l -> 
            let leaves, pi_sets = partition x_set in 

            (* Puisque les deux seuls noeuds de chance sont les deux premiers noeuds où l'on choisit la disposition, p(x) = 0.5*0.5 = 0.25 *)
            List.fold_left (fun sum sset -> 
                match sset with 
                | PI_Set _| Set _ -> failwith "partition est ne fonctionne pas correctement."
                | Leaf (leaf, g) ->
                    let score = 0.25 *. (GameTree.payoff p leaf)  in 
                    sum +. score
                    ) 0. leaves 
            +.
            List.fold_left (fun sum x -> let score= value p pi x depth h max_depth in 
                sum +. score) 0. pi_sets
        | Leaf (leaf,g) -> 0.25 *. (GameTree.payoff p leaf)

    let imp_minimax (opp_str : strategy) (p : player) (my_last_str : strategy) (max_depth : int): float * strategy =
        let h = Hashtbl.create Tools.power_of_2.(15) in 
        let score = 
            match p with 
            | P1 -> value p opp_str (Set [  (GameTree.n11, GameTree.random_beginning) ; 
                                            (GameTree.n12, GameTree.random_beginning) ; 
                                            (GameTree.n21, GameTree.random_beginning) ; 
                                            (GameTree.n22, GameTree.random_beginning) ]) 0 h max_depth
                    
            | P2 -> value p opp_str (Set (List.map (fun n -> 
                                                let opp_alt = opp_str GameTree.random_beginning n in 
                                                GameTree.descend opp_alt n, opp_alt::GameTree.random_beginning) 
                                            [GameTree.n11; GameTree.n21; GameTree.n12; GameTree.n22]
                                    )) 0 h max_depth
        in 
        score, Strategy.create_strategy h my_last_str
    

    
    let opg_imp_minimax (opp_str : strategy) (p : player) (my_last_str : strategy) (max_depth : int): float * strategy =
        let h = Hashtbl.create Tools.power_of_2.(15) in 
        let score = 
            match p with 
            | P1 -> value P1 opp_str (Set [GameTree.root, []]) 0 h max_depth
            | P2 -> let p1_alt = opp_str [] (GameTree.convert_to_info_set [GameTree.root]) in 
                value P2 opp_str (Set [GameTree.descend (p1_alt) GameTree.root, [p1_alt]]) 0 h max_depth
        in 
        score, Strategy.create_strategy h my_last_str


    let rec tpg_value (p : player) (x_set : set) (depth : int) (h : (game * int, alternative) Hashtbl.t) (max_depth : int) : float = 
        match x_set with 
        | PI_Set (x_pi_set,g) -> 
        begin 
            if depth >= max_depth then 0. else (* puisque que l est un ensemble d'information partielle, ça ne peut pas être une feuille donc inutile d'appeler payoff *)
            
            match GameTree.pi_set_player x_pi_set with 
            | p' when p = p' -> 
                (* max {V(extend(Y)) | Y is a child of X } + on met la meilleure alternative dans la stratégie *)
                let rec max_value (l : (set*alternative) list) (maxi : float)  (maxi_alt : alternative) : float= 
                    match l with 
                    | [] ->  
                        Hashtbl.add h (g, Encoding.encode (GameTree.convert_to_info_set x_pi_set)) maxi_alt;
                        maxi
                    | (y_set, alt) :: l' -> let score = tpg_value p y_set (depth+1) h max_depth in 
                        if score > maxi then max_value l' score alt
                        else max_value l' maxi maxi_alt in
                
                let chiildren = GameTree.tpg_children x_pi_set g in 
                max_value (chiildren) (-3.) (-1, -1)
            | _ ->
                (* min {V(extend(Y)) | Y is a child of X }*)
                let rec min_value (l : (set*alternative) list) (mini : float) : float= 
                    match l with 
                    | [] ->  
                        mini
                    | (y_set, alt) :: l' -> let score = tpg_value p y_set depth h max_depth in (* On choisit de n'augmenter la profondeur que pour le joueur max*)
                        if score < mini then min_value l' score
                        else min_value l' mini in
                
                let chiildren = Tools.shuffle_list (GameTree.tpg_children x_pi_set g) in  (* aléatoire pour que l'algorithme ne donne pas toujours la même valeur *)
                min_value (chiildren) (3.)
        end
        | Set l -> 
            let leaves, pi_sets = partition x_set in 

            (* Puisque les deux seuls noeuds de chance sont les deux premiers noeuds où l'on choisit la disposition, p(x) = 0.5*0.5 = 0.25 *)
            List.fold_left (fun sum sset -> 
            match sset with 
            | PI_Set _| Set _ -> failwith "partition est ne fonctionne pas correctement."
            | Leaf (leaf, g) ->
                let score = 0.25 *. (GameTree.payoff p leaf)  in 
                sum +. score
                    ) 0. leaves 
            +.
            List.fold_left (fun sum x -> let score= tpg_value p x depth h max_depth in 
                sum +. score) 0. pi_sets
        | Leaf (leaf,g) -> 0.25 *. (GameTree.payoff p leaf)

    let pure_tpg_imp_minimax (p : player) (depth : int): strategy = 
        let h = Hashtbl.create Tools.power_of_2.(15) in 
        fun (g : game) (i : info_set) -> 
            let code = Encoding.encode i in
            match Hashtbl.find_opt h (g, code) with 
            | Some alt -> alt 
            | None -> let _ = tpg_value p (Set (List.map (fun n -> n,g) (GameTree.convert_to_pi_set i))) 0 h (depth - (List.length g / 2)) in 
                Hashtbl.find h (g, code)

    let mixt_tpg_imp_minimax (p : player) (depth : int): strategy = 
        let h = Hashtbl.create Tools.power_of_2.(15) in 
        fun (g : game) (i : info_set) -> 
            let code = Encoding.encode i in
            match Hashtbl.find_opt h (g, code) with 
            | Some alt -> alt 
            | None -> 
                let x = (GameTree.convert_to_pi_set i)  in 
                match g with 
                | alt1::alt2::g' ->
                    let depth = max (depth - (List.length g / 2) + 1) 1 in
                    let _ = tpg_value p (Set (List.map (fun n -> n,g) x)) 0 h depth in 
                    begin match Hashtbl.find_opt h (g, code) with 
                    | None -> Tools.print_node i; raise Not_found
                    | Some alt -> alt end
                | _ -> (0, -1)

    let prob (x_set : set) : float = 
        match x_set with
        | Set l -> (float (List.length l)) *. 0.25
        | PI_Set (l,g) -> (float (List.length l)) *. 0.25
        | Leaf (n, g) -> 0.25 
    
    exception Return of float

    let imp_alpha_beta (opp_str : strategy) (p : player) (my_last_str : strategy) (max_depth : int) : float * strategy = 
        let h = Hashtbl.create Tools.power_of_2.(15) in 

        let rec max_set ((x,g) : node list * game) (ppp : float) (depth : int) : float = 
            try 
                let best = ref ppp in 
                (* debut ajout *)
                if depth >= max_depth then max 0. !best else begin
                let maxi = ref (-100.0) in (* sert à déterminer maxi_alt, maxi <= best *)
                let maxi_alt = ref (-100, -100) in 
                (* fin ajout *)
                List.iter (fun (child, a) ->
                    let temp = mixed_set child !best (depth+1) in 
                    if temp > !best then (
                        if temp = 1. *. (prob child) then (
                            (* debut ajout *)
                            Hashtbl.add h (g, Encoding.encode (GameTree.convert_to_info_set x)) a;
                            (* fin ajout *)
                            raise (Return temp)
                        );
                        best := temp;
                    );
                    (* debut ajout *)
                    if temp > !maxi then (
                        maxi := temp; 
                        maxi_alt := a;
                    ) (* fin ajout *)
                ) (Tools.shuffle_list (GameTree.opg_children opp_str x g)); (* aléatoire *)
                (* debut ajout *)
                assert (!maxi_alt <> (-100, -100));
                Hashtbl.add h (g, Encoding.encode (GameTree.convert_to_info_set x)) !maxi_alt;
                (* fin ajout *)
                !best end
            with Return temp -> temp

        and mixed_set (x_set : set) (ppp : float) (depth : int): float = 
            try 
                let sum = ref 0. in 
                let prob_left = ref (prob x_set) in 
                let leaves, pi_sets = partition x_set in (* X *)
                
                List.iter (fun x -> 
                    prob_left := !prob_left -. (prob x);
                    begin match x with
                    | Set _ -> failwith "cas impossible"
                    | Leaf (leaf,g) ->
                        sum := !sum +. (prob x) *. (GameTree.payoff p leaf) 
                    | PI_Set (x, g) ->
                        sum := !sum +. max_set (x,g) (ppp -. !sum -. (1. *. !prob_left)) depth end;
                    
                    if !sum +. 1. *. !prob_left <= ppp then 
                        raise (Return ppp)
                ) (leaves@pi_sets);
                !sum 
            with Return ppp -> ppp in

        let score = 
            match p with 
            | P1 -> mixed_set (Set [  (GameTree.n11, GameTree.random_beginning) ; 
                                            (GameTree.n12, GameTree.random_beginning) ; 
                                            (GameTree.n21, GameTree.random_beginning) ; 
                                            (GameTree.n22, GameTree.random_beginning) ]) (-100.) 0
                    
            | P2 -> mixed_set (Set (List.map (fun n -> 
                                                let opp_alt = opp_str GameTree.random_beginning n in 
                                                GameTree.descend opp_alt n, opp_alt::GameTree.random_beginning) 
                                            [GameTree.n11; GameTree.n21; GameTree.n12; GameTree.n22]
                                    )) (-100.) 0 in
        score, Strategy.create_strategy h my_last_str


    let best_response_iteration (p1_str : strategy) (p2_str : strategy) (nb_it : int): strategy * strategy = 
        let p1_str = ref p1_str in 
        let p2_str = ref p2_str in
        let depth = ref 5 in 


        (try 
            for it = 1 to nb_it do 
                (* On cherche une meilleure réponse pour P1 à la stratégie de P2 *)
                let score1, pi = imp_alpha_beta !p2_str P1 !p1_str !depth in 
                p1_str := pi;
                Printf.printf "Itération %d à la profondeur %d : P1 répond à la stratégie de P2 avec une stratégie évaluée à %f.\n" it !depth score1;
                print_newline ();

                let real_score1, real_score2 = UserInterface.test_strategies !p1_str !p2_str (!depth * 2 + 1) in


                if not (Tools.compare_scores score1 (real_score1 -. real_score2) 0.1) then failwith "IMP_Minimax ment pour le score1.\n";

                (* if real_score1 < 0.90 && it > 1 then failwith "On a trouvé une stratégie forte pour le joueur 2.\n"; *)

                flush stdout;

                (* On cherche une meilleure réponse pour P2 à la stratégie de P1 *)
                let score2, mu = imp_alpha_beta !p1_str P2 !p2_str !depth in
                p2_str  := mu;
                Printf.printf "Itération %d à la profondeur %d : P2 répond à la stratégie de P1 avec une stratégie évaluée à %f.\n" it !depth  score2;

                let real_score1, real_score2 = UserInterface.test_strategies !p1_str !p2_str (!depth * 2 + 2) in 

                if not (Tools.compare_scores score2 (real_score2 -. real_score1) 0.1) then failwith "IMP_Minimax ment pour le score2.\n";

                (* if real_score2 < 0.90 && it > 1 then failwith "On a trouvé une stratégie forte pour le joueur 1.\n"; *)

                flush stdout;

                if (Tools.compare_scores (score1 +. score2) 0. 0.1) then
                    (* Peut-être a-ton trouvé un équilibre de Nash ??? *)
                    (* On augment la profondeur au cas où c'est le facteur limitant *)
                    if !depth < 10 then (
                        Printf.printf  "===========================================================
                                        ===========================================================
                                        Equilibre de Nash détecté pour le jeu avec %d coups maximum
                                        ===========================================================
                                        ===========================================================" !depth;
                        depth := !depth + 1
                    ) else 
                        (* Si la profondeur est de 10, on considère que c'est un résultat très intéressant. *)
                        failwith "Equilibre de Nash détecté pour le jeu avec 10 coups maximum !!!"

            done
        with Failure m -> print_string m);
        !p1_str, !p2_str



    exception NashEquilibrium of strategy * strategy
    exception InterestingStrategies of strategy * strategy

    let search_nash_equilibrium_random (nb_tries : int) (max_trial_duration : int) (depth : int) : (strategy * strategy) option = 
        try 
            for ess = 1 to nb_tries do 
                Printf.printf "--- Essai %d : on génère deux nouvelles stratégies au hazard. ---\n" ess;
                Printf.printf "\n";
                flush stdout;
                
                (* A nous de choisir les stratégies initiales : *)
                (* let p1_str = ref (Strategy.create_random_mixt_strategy_2 ()) in 
                let p2_str = ref (Strategy.create_random_mixt_strategy_2 ()) in  *)
                let p1_str = ref (mixt_tpg_imp_minimax P1 depth) in 
                let p2_str = ref (mixt_tpg_imp_minimax P2 depth) in
                for it = 1 to max_trial_duration do 
                    (* On cherche une meilleure réponse pour P1 à la stratégie de P2 *)
                    let score1, pi = imp_alpha_beta !p2_str P1 !p1_str depth in 
                    p1_str := pi;
                    Printf.printf "-Ess%d It%d Prof%d : P1 répond à P2 avec %f.\n" ess it depth score1;

                    if score1 < 0.90 && it > 1 then 
                        Printf.printf "=== ! === On a trouvé une stratégie forte pour le joueur 2. === ! === \n";
                    if score1 < 0.10 && it > 1 then (
                        Printf.printf "======================================================================== \n";
                        Printf.printf "=== ! === On a trouvé une stratégie GAGNANTE pour le joueur 2. === ! === \n";
                        Printf.printf "======================================================================== \n";
                    );
    
                    flush stdout;
    
                    (* On cherche une meilleure réponse pour P2 à la stratégie de P1 *)
                    let score2, mu = imp_alpha_beta !p1_str P2 !p2_str depth in
                    p2_str  := mu;
                    Printf.printf "-Ess%d It%d Prof%d : P2 répond à P1 avec %f.\n" ess it depth score2;
                    
                    if score2 < 0.90 && it > 1 then 
                        Printf.printf "=== ! === On a trouvé une stratégie forte pour le joueur 1. === ! === \n";
                    if score2 < 0.10 && it > 1 then (
                        Printf.printf "======================================================================== \n";
                        Printf.printf "=== ! === On a trouvé une stratégie GAGNANTE pour le joueur 1. === ! === \n";
                        Printf.printf "======================================================================== \n";
                    );
                    flush stdout;
    
                    if (Tools.compare_scores (score1 +. score2) 0. 0.20) then (
                        (* On a trouvé un équilibre de Nash !!!!*)
                        Printf.printf "========================================================================= \n";
                        Printf.printf "========================================================================= \n";
                        Printf.printf "========================== EQUILIBRE DE NASH ============================ \n";
                        Printf.printf "========================================================================= \n";
                        Printf.printf "========================================================================= \n";
                        raise (NashEquilibrium (!p1_str, !p2_str))
                    )
    
                done;
                Printf.printf "\n\n\n";
            done;
            None
        with    
        | NashEquilibrium (p1_str, p2_str) -> Some (p1_str, p2_str)
        | InterestingStrategies (p1_str, p2_str) -> Some (p1_str, p2_str)
    
    exception Abort

    let search_nash_equilibrium_converge (nb_searches : int) (nb_trials : int) (abort_iteration_counts : (float * int) list) (depth : int) : (strategy * strategy) option =
        (* entrée : nb_searches : nombre de recherches. A chaque recherche, de deux stratégies initiales sont inventés et fixes durant tous les essais 
                        de la recherche
                    nb_trials : nombre d'essais par recherche. A chaque essai, on réalise la méthode des meilleures réponses à partir des stratégies initiales de
                        la recherche.
                    abort_iteration_counts : tableau de quand arrêter l'essai en fonction de l'avancement de l'essai (du meilleur score obtenu)
                        exemple : abort_iteration_counts = [(0.99, 50); (0.74, 1000); (0.49, 20000); (0.24, 50000); (-0.01, 200000); (-1.01, 1000000)] 
                    depth : profondeur pour les algorithmes IMP-Minimax utilisés
        Cette fonction recherche un équilibre de Nash en appliquant la méthode des meilleures réponses sur nb_searches couples de stratégies initiales générées
            par IMP-minimax à deux joueurs et nb_trials fois par couple de stratégie initiale. La meilleur réponse est déterminée grâce à l'algorithme 
            IMP-alpha-bêta, la convergence vers un équilibre de Nash est forcée en passant à la stratégie suivante seulement si elle est mieux que la 
            précédente, i.e l'adversaire donne un réponse au pire aussi bien que celle précédente.
        sortie : Un équilibre de Nash au mieux, un stratégie intéressante peut-être, rien sinon. *)
        let delta = 100 in 
        try 
            for search = 1 to nb_searches do 
                Printf.printf "\n\n\n\n\n";
                Printf.printf "--- Recherche %d : on génère deux nouvelles stratégies par l'algorithme IMP-Minimax à deux joueurs. ---\n" search;
                Printf.printf "\n";
                let p1_str = mixt_tpg_imp_minimax P1 depth in 
                let p2_str = mixt_tpg_imp_minimax P2 depth in
                for ess = 1 to nb_trials do 
                    try 
                    Printf.printf "\n\n\n";
                    Printf.printf "--- Recherche %d Essai %d : on essaie une méthode des meilleures réponses avec les stratégies initiales de la recherche %d. ---\n" search ess search;
                    Printf.printf "\n";
                    let global_it = ref 0 in
                    let stuck_it = ref 0 in 

                    let abort_iteration_counts = ref abort_iteration_counts in
                    let rec abort (stuck_it : int) (score : float) : bool = 
                        match !abort_iteration_counts with 
                        | [] -> failwith "Vous n'avez pas précisé de nombre d'itérations pour le score le plus bas possible (ex : -1.01)"
                        | (x, nb_it)::abort_iteration_counts' -> 
                            if score >= x then
                                nb_it <= stuck_it
                            else (* score < x *) (
                                abort_iteration_counts := abort_iteration_counts';
                                abort stuck_it score
                            ) in

                    let rec search_P1 (p1_str : strategy) (p2_str : strategy) (worst_score1 : float) (worst_score2 : float) : unit = 

                        let local_it = ref 1 in
                        let lock = ref false in 
                        while !lock || !local_it <= delta do
                            incr global_it;
                            incr stuck_it;
                            flush stdout; 

                            if abort !stuck_it (min worst_score1 worst_score2) then raise Abort;

                            (* On cherche une meilleure réponse pour P1 à la stratégie de P2 *)
                            let score1, new_p1_str = imp_alpha_beta p2_str P1 p1_str depth in
                            Printf.printf "-Rech%d Ess%d GlobIt%d StuckIt%d Prof%d : P1 répond à P2 avec %f. WScore1 : %f WScore2 : %f\n" search ess !global_it !stuck_it depth score1 worst_score1 worst_score2;
                            
                            if (Tools.compare_scores (score1 +. worst_score2) 0. 0.10) then
                                raise (NashEquilibrium (new_p1_str, p2_str)); (* On a trouvé un équilibre de Nash ! *)
                            
                            if (Tools.compare_scores score1 worst_score1 0.05) then (* La stratégie de P2 précédente aussi bien que l'ancienne. Elle est donc validée. *)
                                search_P2 new_p1_str p2_str score1 worst_score2
                            else if (score1 > worst_score1) then (* La stratégie de P2 précédente est moins bien que l'ancienne. Elle n'est donc pas validée. *)
                                local_it := delta + 1
                            else (* score1 < worst_score1 *) (* La stratégie de P2 précédente est mieux que l'ancienne. Elle est donc validée et bloquée.*)
                            (    
                                lock := true;
                                stuck_it := 0;
                                search_P2 new_p1_str p2_str score1 worst_score2
                            )
                        done
                    and search_P2 (p1_str : strategy) (p2_str : strategy) (worst_score1 : float) (worst_score2 : float) : unit = 
                        let local_it = ref 1 in 
                        let lock = ref false in 
                        while !lock || !local_it <= delta do 
                            incr global_it;
                            incr stuck_it;
                            flush stdout; 

                            if abort !stuck_it (min worst_score1 worst_score2) then raise Abort;

                            (* On cherche une meilleure réponse pour P2 à la stratégie de P1 *)
                            let score2, new_p2_str = imp_alpha_beta p1_str P2 p2_str depth in
                            Printf.printf "-Rech%d Ess%d GlobIt%d StuckIt%d Prof%d : P2 répond à P1 avec %f. WScore1 : %f WScore2 : %f\n" search ess !global_it !stuck_it depth score2 worst_score1 worst_score2;
                            
                            if (Tools.compare_scores (worst_score1 +. score2) 0. 0.10) then
                                raise (NashEquilibrium (p1_str, new_p2_str)); (* On a trouvé un équilibre de Nash ! *)
                                
                            if (Tools.compare_scores score2 worst_score2 0.05) then (* La stratégie de P1 précédente aussi bien que l'ancienne. Elle est donc validée. *)
                                search_P1 p1_str new_p2_str worst_score1 score2
                            else if (score2 > worst_score2) then (* La stratégie de P1 précédente est moins bien que l'ancienne. Elle n'est donc pas validée. *)
                                local_it := delta + 1
                            else (* score2 < worst_score2 *) (* La stratégie de P1 précédente est mieux que l'ancienne. Elle est donc validée et bloquée.*)
                            (    
                                lock := true;
                                stuck_it := 0;
                                search_P1 p1_str new_p2_str worst_score1 score2
                            )
                            
                        done in 
                    
                    search_P1 p1_str p2_str 2.0 2.0
                    with 
                    | Abort -> ()
                done;
            done;
            None
        with    
        | NashEquilibrium (p1_str, p2_str) -> 
            Printf.printf "========================================================================= \n";
            Printf.printf "========================================================================= \n";
            Printf.printf "========================== EQUILIBRE DE NASH ============================ \n";
            Printf.printf "========================================================================= \n";
            Printf.printf "========================================================================= \n";
            Some (p1_str, p2_str)
        | InterestingStrategies (p1_str, p2_str) -> Some (p1_str, p2_str)
        | Abort -> failwith "L'exception Abort ne devrait pas être attrapée ici.\n"

end




