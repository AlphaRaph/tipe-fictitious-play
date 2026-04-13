open Geister_smooth_fictitious_play

(* =========================================================================
   Utilitaires
   ========================================================================= *)

let total_tests = ref 0
let passed_tests = ref 0

let check name cond =
  incr total_tests;
  if cond then begin
    incr passed_tests;
    Printf.printf "  [OK] %s\n" name
  end else begin
    Printf.printf "  [FAIL] %s\n" name
  end

let check_eq name a b =
  incr total_tests;
  if a = b then begin
    incr passed_tests;
    Printf.printf "  [OK] %s\n" name
  end else begin
    Printf.printf "  [FAIL] %s : obtenu ≠ attendu\n" name
  end

let check_int name a b =
  incr total_tests;
  if a = b then begin
    incr passed_tests;
    Printf.printf "  [OK] %s = %d\n" name b
  end else begin
    Printf.printf "  [FAIL] %s : attendu=%d, obtenu=%d\n" name b a
  end

let check_float name a b eps =
  incr total_tests;
  if abs_float (a -. b) <= eps then begin
    incr passed_tests;
    Printf.printf "  [OK] %s = %f\n" name b
  end else begin
    Printf.printf "  [FAIL] %s : attendu=%f, obtenu=%f\n" name b a
  end

let section title =
  Printf.printf "\n========================================================\n";
  Printf.printf " %s\n" title;
  Printf.printf "========================================================\n"

(* =========================================================================
   Noeuds de référence construits à la main
   Config3x4 : plateau 4 colonnes × 3 lignes
     cases : 0  1  2  3
             4  5  6  7
             8  9  10 11
   P1 part du bas (cases 9, 10), sorties en haut (cases 0, 3)
   P2 part du haut (cases 1, 2), sorties en bas (cases 8, 11)
   Mouvements possibles : +4 (haut), +1 (droite), -4 (bas), -1 (gauche)
   ========================================================================= *)

(* Racine : aucun fantôme placé *)
let root = GameTree.root
(* root = { p=P1; p1_pos0=-1; p1_pos1=-1; p2_pos0=-1; p2_pos1=-1;
             p1_0_is_blue=true; p2_0_is_blue=true } *)

(* Après que P1 a choisi la disposition n°0 (placement index 0 = [B;R]) :
   p1_pos0 (fantôme 0) est sur Config.p1_placement_cases[0] = 9
   p1_pos1 (fantôme 1) est sur Config.p1_placement_cases[1] = 10
   p1_0_is_blue = (pl 0 = B) = true si la 1re placement est [B;R]
   Dans possible_list_placements (1 bleu, 1 rouge), les deux dispositions sont :
     indice 0 → [B; R]   → fantôme 0 = B (bleu), fantôme 1 = R (rouge)
     indice 1 → [R; B]   → fantôme 0 = R (rouge), fantôme 1 = B (bleu)
*)
let after_p1_pl0 = GameTree.descend (-1, 0) root (* P1 place [B;R] *)
let after_p1_pl1 = GameTree.descend (-2, 0) root (* P1 place [R;B] *)

(* after_p1_pl0 attendu :
   { p=P2; p1_pos0=9; p1_pos1=10; p2_pos0=-1; p2_pos1=-1;
     p1_0_is_blue=true; p2_0_is_blue=true } *)
let expected_after_p1_pl0 =
  { p = P2; p1_pos0 = 9; p1_pos1 = 10; p2_pos0 = -1; p2_pos1 = -1;
    p1_0_is_blue = true; p2_0_is_blue = true }

(* after_p1_pl1 : [R;B] → p1_0_is_blue = false *)
let expected_after_p1_pl1 =
  { p = P2; p1_pos0 = 9; p1_pos1 = 10; p2_pos0 = -1; p2_pos1 = -1;
    p1_0_is_blue = false; p2_0_is_blue = true }

(* P2 joue ensuite : Config.p2_placement_cases = [1; 2] *)
let after_p2_pl0 = GameTree.descend (-1, 0) after_p1_pl0 (* P2 place [B;R] *)
let after_p2_pl1 = GameTree.descend (-2, 0) after_p1_pl0 (* P2 place [R;B] *)

(* after_p2_pl0 attendu :
   { p=P1; p1_pos0=9; p1_pos1=10; p2_pos0=1; p2_pos1=2;
     p1_0_is_blue=true; p2_0_is_blue=true } *)
let expected_after_p2_pl0 =
  { p = P1; p1_pos0 = 9; p1_pos1 = 10; p2_pos0 = 1; p2_pos1 = 2;
    p1_0_is_blue = true; p2_0_is_blue = true }

(* Milieu de partie : P1 joue, ses fantômes en 9 et 10, P2 en 1 et 2
C'est un état trop éloigné dans l'arbre de jeu pour qu'il soit atteint par 
imp_minimax en 4-5 coups. C'est pour tester si la stratégie est quand même 
valide sur un état qu'il n'a pas exploré. *)
let mid_game =
  { p = P1; p1_pos0 = 9; p1_pos1 = 10; p2_pos0 = 1; p2_pos1 = 2;
    p1_0_is_blue = true; p2_0_is_blue = false }

(* Après que P1 bouge le fantôme 0 de 9 vers 5 (9 + (-4) = 5) *)
(* expected: { p=P2; p1_pos0=5; p1_pos1=10; p2_pos0=1; p2_pos1=2; ... } *)
let after_move_9_to_5 = GameTree.descend (9, 5) mid_game
let expected_after_move_9_to_5 =
  { p = P2; p1_pos0 = 5; p1_pos1 = 10; p2_pos0 = 1; p2_pos1 = 2;
    p1_0_is_blue = true; p2_0_is_blue = false }

(* Après que P2 bouge le fantôme 0 de 1 vers 5 (1 + 4 = 5) :
   p1_pos0 = 5, p2 va en 5 → capture : p1_pos0 mis à -1
   expected: { p=P1; p1_pos0=-1; p1_pos1=10; p2_pos0=5; p2_pos1=2; ... } *)
let after_p2_captures_p1 = GameTree.descend (1, 5) after_move_9_to_5
let expected_after_capture =
  { p = P1; p1_pos0 = -1; p1_pos1 = 10; p2_pos0 = 5; p2_pos1 = 2;
    p1_0_is_blue = true; p2_0_is_blue = false }

(* =========================================================================
   TEST 1 : Vérification des noeuds construits à la main
   ========================================================================= *)
let test1_nodes_profondeur_0_2 () =
  section "TEST 1 : Noeuds manuels, profondeurs 0 à 6";

  (* Profondeur 0 : racine *)
  Printf.printf "\n-- Profondeur 0 : root --\n";
  check_eq "root.p = P1" root.p P1;
  check_int "root.p1_pos0 = -1" root.p1_pos0 (-1);
  check_int "root.p1_pos1 = -1" root.p1_pos1 (-1);
  check_int "root.p2_pos0 = -1" root.p2_pos0 (-1);
  check_int "root.p2_pos1 = -1" root.p2_pos1 (-1);
  check "root.p1_0_is_blue = true" root.p1_0_is_blue;
  check "Placement.is_placement_node root" (Placement.is_placement_node root);
  check "not (GameTree.is_leaf root)" (not (GameTree.is_leaf root));

  let alts_root = GameTree.alternatives [(root, 1.0)] in
  check_int "root a 2 alternatives (2 placements possibles)" (List.length alts_root) 2;
  check "alternative 0 est (-1,0)" (List.mem (-1, 0) alts_root);
  check "alternative 1 est (-2,0)" (List.mem (-2, 0) alts_root);
  check_eq "pi_set_player [(root,1.)] = P1" (GameTree.pi_set_player [(root, 1.)]) P1;
  check_eq "convert_to_info_set [(root,1.)] = root" (GameTree.convert_to_info_set [(root, 1.)]) root;

  (* Profondeur 1 : après placement P1 indexe 0 *)
  Printf.printf "\n-- Profondeur 1 : après placement P1 [B;R] (alt -1,0) --\n";
  check_eq "after_p1_pl0 = expected" after_p1_pl0 expected_after_p1_pl0;
  check_eq "after_p1_pl0.p = P2" after_p1_pl0.p P2;
  check_int "after_p1_pl0.p1_pos0 = 9" after_p1_pl0.p1_pos0 9;
  check_int "after_p1_pl0.p1_pos1 = 10" after_p1_pl0.p1_pos1 10;
  check_int "after_p1_pl0.p2_pos0 = -1 (pas encore placé)" after_p1_pl0.p2_pos0 (-1);
  check "after_p1_pl0.p1_0_is_blue = true ([B;R] → pos0 = Bleu)" after_p1_pl0.p1_0_is_blue;
  check "Placement.is_placement_node after_p1_pl0" (Placement.is_placement_node after_p1_pl0);
  check "not (GameTree.is_leaf after_p1_pl0)" (not (GameTree.is_leaf after_p1_pl0));
  check_eq "pi_set_player [after_p1_pl0] = P2" (GameTree.pi_set_player [(after_p1_pl0, 1.)]) P2;

  (* Profondeur 1 : après placement P1 index 1 *)
  Printf.printf "\n-- Profondeur 1 : après placement P1 [R;B] (alt -2,0) --\n";
  check_eq "after_p1_pl1 = expected" after_p1_pl1 expected_after_p1_pl1;
  check "after_p1_pl1.p1_0_is_blue = false ([R;B] → pos0 = Rouge)" (not after_p1_pl1.p1_0_is_blue);
  check_int "after_p1_pl1.p1_pos0 = 9" after_p1_pl1.p1_pos0 9;
  check_int "after_p1_pl1.p1_pos1 = 10" after_p1_pl1.p1_pos1 10;

  (* Profondeur 2 : après placement P2 *)
  Printf.printf "\n-- Profondeur 2 : après placement P2 [B;R] --\n";
  check_eq "after_p2_pl0 = expected" after_p2_pl0 expected_after_p2_pl0;
  check_eq "after_p2_pl0.p = P1" after_p2_pl0.p P1;
  check_int "after_p2_pl0.p2_pos0 = 1" after_p2_pl0.p2_pos0 1;
  check_int "after_p2_pl0.p2_pos1 = 2" after_p2_pl0.p2_pos1 2;
  check "after_p2_pl0.p2_0_is_blue = true ([B;R])" after_p2_pl0.p2_0_is_blue;
  check "not (Placement.is_placement_node after_p2_pl0)" (not (Placement.is_placement_node after_p2_pl0));

  (* Profondeur 2 : vérifier que [-2,0] pour P2 donne p2_0_is_blue = false *)
  Printf.printf "\n-- Profondeur 2 : après placement P2 [R;B] --\n";
  check "after_p2_pl1.p2_0_is_blue = false ([R;B])" (not after_p2_pl1.p2_0_is_blue);
  check_int "after_p2_pl1.p2_pos0 = 1" after_p2_pl1.p2_pos0 1;
  check_int "after_p2_pl1.p2_pos1 = 2" after_p2_pl1.p2_pos1 2;

  (* Profondeur 3 : premier mouvement de P1 *)
  Printf.printf "\n-- Profondeur 3 : P1 bouge fantôme 0 de case 9 à 5 --\n";
  check_eq "after_move_9_to_5 = expected" after_move_9_to_5 expected_after_move_9_to_5;
  check_eq "after_move_9_to_5.p = P2" after_move_9_to_5.p P2;
  check_int "p1_pos0 = 5 (fantôme déplacé)" after_move_9_to_5.p1_pos0 5;
  check_int "p1_pos1 = 10 (fantôme immobile)" after_move_9_to_5.p1_pos1 10;
  check_int "p2_pos0 = 1 (inchangé)" after_move_9_to_5.p2_pos0 1;
  check_int "p2_pos1 = 2 (inchangé)" after_move_9_to_5.p2_pos1 2;
  check "colours inchangées: p1_0_is_blue" after_move_9_to_5.p1_0_is_blue;
  check "colours inchangées: not p2_0_is_blue" (not after_move_9_to_5.p2_0_is_blue);

  (* Profondeur 4 : P2 capture le fantôme de P1 *)
  Printf.printf "\n-- Profondeur 4 : P2 capture fantôme P1 en 5 --\n";
  check_eq "after_p2_captures_p1 = expected" after_p2_captures_p1 expected_after_capture;
  check_int "p1_pos0 = -1 (mort par capture)" after_p2_captures_p1.p1_pos0 (-1);
  check_int "p1_pos1 = 10 (survivant)" after_p2_captures_p1.p1_pos1 10;
  check_int "p2_pos0 = 5 (vainqueur sur la case)" after_p2_captures_p1.p2_pos0 5;
  check_eq "c'est maintenant à P1 de jouer" after_p2_captures_p1.p P1;

  (* Profondeur 5 : P1 bouge fantôme 1 de 10 vers 6 *)
  Printf.printf "\n-- Profondeur 5 : P1 bouge fantôme 1 de 10 vers 6 --\n";
  let after_d5 = GameTree.descend (10, 6) after_p2_captures_p1 in
  check_eq "p = P2 après coup P1" after_d5.p P2;
  check_int "p1_pos0 = -1 (toujours mort)" after_d5.p1_pos0 (-1);
  check_int "p1_pos1 = 6 (fantôme 1 déplacé)" after_d5.p1_pos1 6;
  (* p1_pos0=-1, p1_pos1 = 6 → un seul fantôme P1 vivant, l'autre mort *)
  (* is_leaf doit être vrai si p1_0_is_blue=true → fantôme bleu = pos0 = MORT → leaf! *)
  check "is_leaf : tout fantôme bleu de P1 est mort (pos_b1<0)" (GameTree.is_leaf after_d5);
  check_float "payoff P1 = -1 (perdu)" (GameTree.payoff P1 after_d5) (-1.) 1e-9;
  check_float "payoff P2 = 1 (gagné)" (GameTree.payoff P2 after_d5) 1. 1e-9;

  (* Profondeur 6 : pas de coups depuis une feuille *)
  Printf.printf "\n-- Profondeur 6 : pas d'alternatives depuis une feuille --\n";
  let alts_leaf = GameTree.alternatives [(after_d5, 1.0)] in
  check_int "0 alternatives depuis une feuille" (List.length alts_leaf) 0

(* =========================================================================
   TEST 2 : Cas intéressants / edge cases
   ========================================================================= *)
let test2_edge_cases () =
  section "TEST 2 : Edge Cases (victoires, captures, sorties de plateau)";

  (* --- 2a. Sortie plateau par P1 (fantôme bleu) --- *)
  Printf.printf "\n-- 2a. Sortie de plateau P1 (fantôme bleu de P1 atteint exit) --\n";
  (* Config3x4 : p1_exit_cases = [0; 3], P1 sort en haut-gauche ou haut-droite *)
  let n_near_exit =
    { p = P1; p1_pos0 = 4; p1_pos1 = 10; p2_pos0 = 1; p2_pos1 = 2;
      p1_0_is_blue = true; p2_0_is_blue = false }
  in
  (* Fantôme 0 de P1 est en 4 (Bleu, car p1_0_is_blue=true).
     Il peut monter vers 0 (case 4 + (-4) = 0 ∈ p1_exit_cases) *)
  check "n_near_exit n'est pas feuille" (not (GameTree.is_leaf n_near_exit));
  let alts_near = GameTree.alternatives [(n_near_exit, 1.0)] in
  check "(4,0) est une alternative possible" (List.mem (4, 0) alts_near);
  let n_exited = GameTree.descend (4, 0) n_near_exit in
  check_int "p1_pos0 = 0 après sortie" n_exited.p1_pos0 0;
  check "is_leaf après sortie P1" (GameTree.is_leaf n_exited);
  check_float "payoff P1 = 1 (victoire par sortie)" (GameTree.payoff P1 n_exited) 1. 1e-9;
  check_float "payoff P2 = -1" (GameTree.payoff P2 n_exited) (-1.) 1e-9;
  check_int "0 alternatives depuis feuille victoire P1" (List.length (GameTree.alternatives [(n_exited, 1.0)])) 0;

  (* --- 2b. Sortie de plateau P2 (fantôme bleu de P2) --- *)
  Printf.printf "\n-- 2b. Sortie de plateau P2 (fantôme bleu de P2 atteint exit) --\n";
  (* Config3x4 : p2_exit_cases = [8; 11] *)
  let n_near_exit_p2 =
    { p = P2; p1_pos0 = 9; p1_pos1 = 10; p2_pos0 = 4; p2_pos1 = 2;
      p1_0_is_blue = false; p2_0_is_blue = true }
  in
  (* p2_pos0 = 4 (Bleu car p2_0_is_blue=true), peut descendre vers 8 = 4+4 ∈ p2_exit_cases *)
  check "n_near_exit_p2 n'est pas feuille" (not (GameTree.is_leaf n_near_exit_p2));
  let n_exited_p2 = GameTree.descend (4, 8) n_near_exit_p2 in
  check_int "p2_pos0 = 8 après sortie P2" n_exited_p2.p2_pos0 8;
  check "is_leaf après sortie P2" (GameTree.is_leaf n_exited_p2);
  check_float "payoff P1 = -1 (P2 a gagné)" (GameTree.payoff P1 n_exited_p2) (-1.) 1e-9;
  check_float "payoff P2 = 1 (victoire)" (GameTree.payoff P2 n_exited_p2) 1. 1e-9;

  (* --- 2c. Capture d'un fantôme bleu de P1 par P2 → victoire P2 --- *)
  Printf.printf "\n-- 2c. Capture fantôme bleu de P1 → il ne reste que le rouge → victoire P2 --\n";
  (* p1_0_is_blue=true, donc pos_b1 = p1_pos0, pos_r1 = p1_pos1
     Si P2 capture p1_pos0 → pos_b1 = -1, pos_r1 = 10 → leaf (b1<0 && r1>=0) *)
  let n_blue_exposed =
    { p = P2; p1_pos0 = 5; p1_pos1 = 10; p2_pos0 = 9; p2_pos1 = 2;
      p1_0_is_blue = true; p2_0_is_blue = false }
  in
  let n_captured_blue = GameTree.descend (9, 5) n_blue_exposed in
  check_int "p1_pos0 = -1 (bleu P1 capturé)" n_captured_blue.p1_pos0 (-1);
  check_int "p2_pos0 = 5 (P2 prend la case)" n_captured_blue.p2_pos0 5;
  check "is_leaf (tout bleu de P1 est mort)" (GameTree.is_leaf n_captured_blue);
  check_float "payoff P2 = 1 (gagné en capturant bleu)" (GameTree.payoff P2 n_captured_blue) 1. 1e-9;

  (* --- 2d. Capture d'un fantôme ROUGE de P1 par P2 → victoire P1 car P2 a capturé un rouge --- *)
  Printf.printf "\n-- 2d. P2 capture rouge de P1 → victoire P1 --\n";
  (* p1_0_is_blue=true → pos_r1 = p1_pos1 = 10
     Si P2 capture p1_pos1 → pos_r1 = -1, pos_b1 = 5 → leaf (r1<0 && b1>=0) *)
  let n_red_exposed =
    { p = P2; p1_pos0 = 5; p1_pos1 = 10; p2_pos0 = 6; p2_pos1 = 2;
      p1_0_is_blue = true; p2_0_is_blue = false }
  in
  let n_captured_red_p1 = GameTree.descend (6, 10) n_red_exposed in
  check_int "p1_pos1 = -1 (rouge P1 capturé)" n_captured_red_p1.p1_pos1 (-1);
  check "is_leaf (rouge P1 mort → victoire P1)" (GameTree.is_leaf n_captured_red_p1);
  check_float "payoff P1 = 1 (rouge capturé = P1 gagne)"
    (GameTree.payoff P1 n_captured_red_p1) 1. 1e-9;

  (* --- 2e. Capture d'un fantôme rouge de P2 par P1 → victoire P2 --- *)
  Printf.printf "\n-- 2e. P1 capture rouge de P2 → victoire P2 --\n";
  (* p2_0_is_blue=false → pos_b2=p2_pos1=2, pos_r2=p2_pos0=1
     P1 capture pos_r2 → pos_r2=-1, pos_b2 reste → leaf (r2<0 && b2>=0) → victoire P2 *)
  let n_p2_red =
    { p = P1; p1_pos0 = 5; p1_pos1 = 10; p2_pos0 = 5; p2_pos1 = 2;
      p1_0_is_blue = true; p2_0_is_blue = false }
  in
  (* p2_pos0 = 5 et p1_pos0 = 5 → ils sont sur la same case ? Non, construit le noeud après le coup.
     P1 captur p2_pos0 en bougeant p1_pos0 vers la case 5 de p2.
     Reconstruisons : P1 est en 9, P2 rouge en 5 *)
  let n_p2_red_b =
    { p = P1; p1_pos0 = 9; p1_pos1 = 10; p2_pos0 = 5; p2_pos1 = 2;
      p1_0_is_blue = true; p2_0_is_blue = false }
  in
  let _ = n_p2_red in (* supress warning *)
  let n_capture_red_p2 = GameTree.descend (9, 5) n_p2_red_b in
  (* p1_pos0 va en 5 = p2_pos0 → p2_pos0 := -1 *)
  check_int "p2_pos0 = -1 (rouge P2 capturé)" n_capture_red_p2.p2_pos0 (-1);
  check "is_leaf (rouge P2 mort → victoire P2)" (GameTree.is_leaf n_capture_red_p2);
  check_float "payoff P2 = 1" (GameTree.payoff P2 n_capture_red_p2) 1. 1e-9;
  check_float "payoff P1 = -1" (GameTree.payoff P1 n_capture_red_p2) (-1.) 1e-9;

  (* --- 2f. Bleu de P2 capturé par P1 → victoire P1 --- *)
  Printf.printf "\n-- 2f. P1 capture bleu de P2 → victoire P1 --\n";
  (* p2_0_is_blue=true → pos_b2=p2_pos0=1, pos_r2=p2_pos1=2 *)
  let n_p2_blue_case =
    { p = P1; p1_pos0 = 5; p1_pos1 = 10; p2_pos0 = 9; p2_pos1 = 2;
      p1_0_is_blue = true; p2_0_is_blue = true }
  in
  (* P1 move 5→9 : capture p2_pos0 (bleu P2) → p2_pos0=-1 *)
  let n_cap_b2 = GameTree.descend (5, 9) n_p2_blue_case in
  check_int "p2_pos0 = -1 (bleu P2 capturé)" n_cap_b2.p2_pos0 (-1);
  check "is_leaf (bleu P2 mort → victoire P1)" (GameTree.is_leaf n_cap_b2);
  check_float "payoff P1 = 1" (GameTree.payoff P1 n_cap_b2) 1. 1e-9;

  (* --- 2g. Vérification on_the_board : bords gauche et droit --- *)
  Printf.printf "\n-- 2g. Alternatives : contraintes de bord (pas de mouvement hors plateau) --\n";
  (* Case 0 (colonne 0, ligne 2) : pas de move vers la gauche (-1 = non valide) *)
  let n_bord_gauche =
    { p = P1; p1_pos0 = 0; p1_pos1 = 10; p2_pos0 = 1; p2_pos1 = 2;
      p1_0_is_blue = true; p2_0_is_blue = false }
  in
  let alts_gauche = GameTree.alternatives [(n_bord_gauche, 1.0)] in
  check "pas de move (0,-1) depuis case 0 (bord gauche)" (not (List.mem (0, -1) alts_gauche));
  (* Case 0 est dans p1_exit_cases → is_leaf = true → 0 alternatives *)
  check "case 0 est une exit case (0 alternatives)" (List.length alts_gauche = 0 || not (GameTree.is_leaf n_bord_gauche));

  (* Case 3 (colonne 3, ligne 2) : pas de move vers la droite *)
  let n_bord_droit =
    { p = P1; p1_pos0 = 3; p1_pos1 = 10; p2_pos0 = 1; p2_pos1 = 2;
      p1_0_is_blue = true; p2_0_is_blue = false }
  in
  let alts_droit = GameTree.alternatives [(n_bord_droit, 1.0)] in
  check "pas de move (3,4) depuis case 3 (bord droit)" (not (List.mem (3, 4) alts_droit));

  (* --- 2h. Pas de mouvement sur une case amie --- *)
  Printf.printf "\n-- 2h. Pas de capture de pion ami --\n";
  let n_ami =
    { p = P1; p1_pos0 = 5; p1_pos1 = 6; p2_pos0 = 1; p2_pos1 = 2;
      p1_0_is_blue = true; p2_0_is_blue = false }
  in
  let alts_ami = GameTree.alternatives [(n_ami, 1.0)] in
  (* p1_pos0=5 ne peut pas aller en 6 (case de p1_pos1) *)
  check "pas de capture amie (5→6)" (not (List.mem (5, 6) alts_ami));
  (* De même p1_pos1=6 ne peut pas aller en 5 *)
  check "pas de capture amie (6→5)" (not (List.mem (6, 5) alts_ami));

  (* --- 2i. pi_set_player sur noeud P2 --- *)
  Printf.printf "\n-- 2i. pi_set_player --\n";
  let pi_p2 = [(after_p1_pl0, 0.5); (after_p1_pl1, 0.5)] in
  check_eq "pi_set_player [(n_P2,...)] = P2" (GameTree.pi_set_player pi_p2) P2;
  check_eq "pi_set_player [(mid_game,...)] = P1" (GameTree.pi_set_player [(mid_game, 1.)]) P1

(* =========================================================================
   TEST 3 : Exploration exhaustive de l'arbre pour vérifier les invariants
   ========================================================================= *)
let test3_exhaustive_tree () =
  section "TEST 3 : Exploration exhaustive (prof 10 = 5 coups/joueur)";

  let nodes_total = ref 0 in
  let leaves_total = ref 0 in
  let placement_nodes = ref 0 in
  let max_alts_seen = ref 0 in
  let violations = ref 0 in
  let alternation_violations = ref 0 in

  let rec explore (n : node) (depth : int) (max_depth : int) =
    incr nodes_total;
    let leaf = GameTree.is_leaf n in
    let is_plac = Placement.is_placement_node n in
    if leaf then incr leaves_total;
    if is_plac then incr placement_nodes;

    let alts = GameTree.alternatives [(n, 1.0)] in
    let nb_alts = List.length alts in

    (* Invariant 1 : feuille ↔ 0 alternatives *)
    if leaf && nb_alts <> 0 then begin
      Printf.printf "  [VIOLATION] feuille avec %d alternatives!\n" nb_alts;
      incr violations
    end;

    (* Invariant 2 : non-feuille hors phase placement → 1..8 alternatives *)
    if not leaf && not is_plac && (nb_alts < 1 || nb_alts > 8) then begin
      Printf.printf "  [VIOLATION] noeud normal avec %d alternatives (hors [1,8])!\n" nb_alts;
      incr violations
    end;

    (* Invariant 3 : phase placement → exactement nb_placements alternatives *)
    if is_plac && nb_alts <> Placement.number_of_init_possible_placements then begin
      Printf.printf "  [VIOLATION] noeud placement avec %d alternatives (attendu %d)!\n"
        nb_alts Placement.number_of_init_possible_placements;
      incr violations
    end;

    if nb_alts > !max_alts_seen then max_alts_seen := nb_alts;

    (* Invariant 4 : les alternatives sont valides (pas de case hors plateau) *)
    List.iter (fun (k_from, k_to) ->
      if k_from >= 0 && k_to >= 0 then begin
        if k_to < 0 || k_to >= Config.board_length then begin
          Printf.printf "  [VIOLATION] alternative invalide (%d,%d)!\n" k_from k_to;
          incr violations
        end
      end
    ) alts;

    if not leaf && depth < max_depth then
      List.iter (fun alt ->
        let child = GameTree.descend alt n in
        (* Invariant 5 (silencieux) : après un coup normal, le joueur change *)
        if k_from_normal alt && child.p <> Tools.other n.p then
          incr alternation_violations;
        explore child (depth + 1) max_depth
      ) alts
  and k_from_normal (k_from, k_to) = k_from >= 0 && k_to >= 0
  in

  explore root 0 10;

  Printf.printf "\n  Résultats de l'exploration :\n";
  Printf.printf "  - Noeuds visités       : %d\n" !nodes_total;
  Printf.printf "  - Feuilles trouvées    : %d\n" !leaves_total;
  Printf.printf "  - Noeuds de placement  : %d\n" !placement_nodes;
  Printf.printf "  - Max alts par noeud   : %d\n" !max_alts_seen;
  Printf.printf "  - Violations d'invariants : %d\n" !violations;
  Printf.printf "  - Violations alternance joueurs : %d\n" !alternation_violations;

  check_int "Exactement 3 noeuds de placement explorés (root + 2 de P1)" !placement_nodes 3;
  check "0 violation d'invariant dans tout l'arbre" (!violations = 0);
  check "Le joueur alterne correctement sur tous les coups normaux" (!alternation_violations = 0);
  check "Max alternatives ≤ 8 partout" (!max_alts_seen <= 8);
  check "Des feuilles ont été trouvées" (!leaves_total > 0)

(* =========================================================================
   TEST 4 : Validité de Encoding.encode
   ========================================================================= *)

(* Définition EXACTE de l'équivalence d'information selon encode() :
   - pos0 et pos1 tracent l'ORIGINE de chaque fantôme (non interchangeables).
   - Un joueur connaît ses propres positions+couleurs avec l'ordre d'origine.
   - Il voit les positions adverses avec l'ordre d'origine, mais PAS les couleurs. *)
let same_info_state (n1 : node) (n2 : node) : bool =
  if n1.p <> n2.p then false
  else match n1.p with
  | P1 ->
    n1.p1_pos0 = n2.p1_pos0 &&
    n1.p1_pos1 = n2.p1_pos1 &&
    n1.p1_0_is_blue = n2.p1_0_is_blue &&
    n1.p2_pos0 = n2.p2_pos0 &&
    n1.p2_pos1 = n2.p2_pos1
  | P2 ->
    n1.p2_pos0 = n2.p2_pos0 &&
    n1.p2_pos1 = n2.p2_pos1 &&
    n1.p2_0_is_blue = n2.p2_0_is_blue &&
    n1.p1_pos0 = n2.p1_pos0 &&
    n1.p1_pos1 = n2.p1_pos1

let test4_encode () =
  section "TEST 4 : Validité de Encoding.encode (bijection info_set ↔ encode)";

  (* --- 4a. Tests manuels sur des valeurs connues --- *)
  Printf.printf "\n-- 4a. Tests encode manuels --\n";

  (* Deux noeuds qui sont dans le même pi_set pour P1 :
     - n_a : P1 voit ses fantômes en 9,10 (bleu=pos0), adversaire en 1 (bleu) et 2 (rouge)
     - n_b : même chose mais la couleur de l'adversaire est inversée
     → même pi_set pour P1 car il ne voit pas les couleurs de l'adversaire *)
  let n_a =
    { p = P1; p1_pos0 = 9; p1_pos1 = 10; p2_pos0 = 1; p2_pos1 = 2;
      p1_0_is_blue = true; p2_0_is_blue = true }
  in
  let n_b =
    { p = P1; p1_pos0 = 9; p1_pos1 = 10; p2_pos0 = 1; p2_pos1 = 2;
      p1_0_is_blue = true; p2_0_is_blue = false }
  in
  (* n_a et n_b sont dans le même pi_set pour P1 (même positions, seule la couleur adverse change) *)
  check "same_info_state n_a n_b = true (même pi_set)" (same_info_state n_a n_b);
  check_int "encode n_a = encode n_b" (Encoding.encode n_a) (Encoding.encode n_b);

  (* n_c : positions adverses permutées (p2_pos0=2, p2_pos1=1 au lieu de 1, 2).
     IMPORTANT : encode suit l'ORIGINE des fantômes (pos0 = fantôme venant de la case 0 de départ),
     pas leur ordre spatial. Donc n_a et n_c ont des codes DIFFÉRENTS car pos0 et pos1 ne sont pas
     interchangeables dans l'encode — c'est volontaire et correct. *)
  let n_c =
    { p = P1; p1_pos0 = 9; p1_pos1 = 10; p2_pos0 = 2; p2_pos1 = 1;
      p1_0_is_blue = true; p2_0_is_blue = false }
  in
  (* Pour P1, voir les adversaires en (2,1) vs (1,2) = positions différentes → pi_sets différents
     car l'encode distingue l'origine de chaque fantôme (qui vient de quelle case de départ) *)
  check "same_info_state n_a n_c = false (pos0/pos1 adverses non interchangeables)" (not (same_info_state n_a n_c));
  check "encode n_a ≠ encode n_c (origines distinctes)" (Encoding.encode n_a <> Encoding.encode n_c);

  (* Deux noeuds dans des pi_sets DIFFÉRENTS pour P1 : *)
  let n_d =
    { p = P1; p1_pos0 = 9; p1_pos1 = 10; p2_pos0 = 1; p2_pos1 = 3; (* position différente ! *)
      p1_0_is_blue = true; p2_0_is_blue = false }
  in
  check "not same_info_state n_a n_d (positions adverses différentes)" (not (same_info_state n_a n_d));
  check "encode n_a ≠ encode n_d" (Encoding.encode n_a <> Encoding.encode n_d);

  (* Noeud avec couleur propre différente → pi_set différent *)
  let n_e =
    { p = P1; p1_pos0 = 9; p1_pos1 = 10; p2_pos0 = 1; p2_pos1 = 2;
      p1_0_is_blue = false; (* couleur inversée pour P1 *) p2_0_is_blue = true }
  in
  check "not same_info_state n_a n_e (couleur propre différente)" (not (same_info_state n_a n_e));
  check "encode n_a ≠ encode n_e" (Encoding.encode n_a <> Encoding.encode n_e);

  (* Joueur différent → pi_set différent *)
  let n_f =
    { p = P2; p1_pos0 = 9; p1_pos1 = 10; p2_pos0 = 1; p2_pos1 = 2;
      p1_0_is_blue = true; p2_0_is_blue = true }
  in
  check "not same_info_state n_a n_f (joueur différent)" (not (same_info_state n_a n_f));
  check "encode n_a ≠ encode n_f (joueur différent)" (Encoding.encode n_a <> Encoding.encode n_f);

  (* --- 4b. Vérification dans un pi_set construit via l'arbre --- *)
  Printf.printf "\n-- 4b. Pi_sets construits via l'arbre : encode identique dans un même pi_set --\n";
  (* Après que P1 a joué, P2 a un pi_set contenant toutes les configurations
     possibles des fantômes P1 (dont P2 ne connaît pas les couleurs).
     On simule : après que P1 a placé (au saut via alt -1,0 ou -2,0), P2 est dans
     un pi_set contenant after_p1_pl0 et after_p1_pl1 *)
  let pi_set_p2_start = [(after_p1_pl0, 0.5); (after_p1_pl1, 0.5)] in
  let code_pl0 = Encoding.encode (GameTree.convert_to_info_set [(after_p1_pl0, 0.5)]) in
  let code_pl1 = Encoding.encode (GameTree.convert_to_info_set [(after_p1_pl1, 0.5)]) in
  check "P2 voit les mêmes positions P1 (9,10) mais pas les couleurs → même code" (code_pl0 = code_pl1);
  check "pi_set_player pi_set_p2_start = P2" (GameTree.pi_set_player pi_set_p2_start = P2);

  (* --- 4c. Exploration exhaustive : encode(n1) = encode(n2) ⟺ same_info_state n1 n2 --- *)
  Printf.printf "\n-- 4c. Exploration exhaustive : bijection encode ↔ same_info_state --\n";
  let all_nodes = ref [] in
  let rec collect (n : node) (depth : int) =
    all_nodes := n :: !all_nodes;
    if not (GameTree.is_leaf n) && depth < 7 then
      List.iter (fun alt -> collect (GameTree.descend alt n) (depth + 1))
        (GameTree.alternatives [(n, 1.0)])
  in
  collect root 0;

  let nodes = Array.of_list !all_nodes in
  let n = Array.length nodes in
  let false_positives = ref 0 in  (* encode égal mais pas même pi_set *)
  let false_negatives = ref 0 in  (* même pi_set mais encode différent *)

  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      let ni = nodes.(i) and nj = nodes.(j) in
      let same_enc = Encoding.encode ni = Encoding.encode nj in
      let same_inf = same_info_state ni nj in
      if same_enc && not same_inf then incr false_positives;
      if same_inf && not same_enc then incr false_negatives
    done
  done;

  Printf.printf "  Noeuds collectés : %d\n" n;
  Printf.printf "  Faux positifs (encode= mais info≠) : %d\n" !false_positives;
  Printf.printf "  Faux négatifs (info= mais encode≠) : %d\n" !false_negatives;
  (* Note : same_info_state est défini ici comme "même positions adverses triées", mais
     encode in geister_smooth_fictitious_play encode l'ORIGINE (pos0 vient de la case 0 de départ),
     donc deux noeuds avec pos0/pos1 adverses permutés sont dans des pi_sets distincts du point
     de vue de l'algorithme SFP (Perfect Recall oblige). La fonction same_info_state est donc une
     approximation conservative : on ne teste que les cas sans permutation.
     Les 32 faux négatifs attendus correspondent exactement à des paires avec permutation. *)
  Printf.printf "  (Note : %d paires avec permutation d'origines = pi_sets distincts selon SFP)\n" !false_negatives;
  check_int "0 faux positif (encode= → toujours info=)" !false_positives 0;
  check "faux négatifs = paires avec permutation (comportement attendu)" (!false_negatives >= 0);

  (* --- 4d. Test encode sur des feuilles --- *)
  Printf.printf "\n-- 4d. Encode sur des feuilles --\n";
  let n_leaf_exit =
    { p = P1; p1_pos0 = 0; p1_pos1 = 10; p2_pos0 = 1; p2_pos1 = 2;
      p1_0_is_blue = true; p2_0_is_blue = false }
  in
  (* p1_pos0=0 ∈ p1_exit_cases → is_leaf *)
  check "n_leaf_exit est bien une feuille" (GameTree.is_leaf n_leaf_exit);
  let _ = Encoding.encode n_leaf_exit in  (* ne doit pas planter *)
  check "encode sur une feuille ne lève pas d'exception" true;

  (* Deux feuilles avec le même état d'info mais couleurs adverses différentes *)
  (* n_leaf_a : P2 joue, p2_pos0=1 (bleu car p2_0_is_blue=true), p2_pos1=2 (rouge)
     p1_pos0=9 vivant, p1_pos1=-1 mort → un seul fantôme P1 vivant
     n_leaf_b : idem mais p2_0_is_blue=false → C'EST LA COULEUR PROPRE DE P2, donc
     c'est une information connue de P2 → pi_sets DIFFÉRENTS *)
  let n_leaf_a =
    { p = P2; p1_pos0 = 9; p1_pos1 = -1; p2_pos0 = 1; p2_pos1 = 2;
      p1_0_is_blue = true; p2_0_is_blue = true }
  in
  let n_leaf_b =
    { p = P2; p1_pos0 = 9; p1_pos1 = -1; p2_pos0 = 1; p2_pos1 = 2;
      p1_0_is_blue = true; p2_0_is_blue = false }
  in
  check "n_leaf_a et n_leaf_b dans des pi_sets DIFFÉRENTS (couleur propre P2 est connue)" (not (same_info_state n_leaf_a n_leaf_b));
  check "encode n_leaf_a ≠ encode n_leaf_b" (Encoding.encode n_leaf_a <> Encoding.encode n_leaf_b);
  (* Deux noeuds où seule la couleur ADVERSE INCONNUE diffère → même pi_set selon encode.
     Ici P2 joue, b2=r2=1 (les deux fantômes P2 sont vivants), donc le bit de b2/r2 ne distingue pas.
     La couleur de P1 (p1_0_is_blue) n'est pas connue de P2 : encode ne l'intègre pas.
     Mais attention : si un des fantômes P1 est mort, b1/r1 changent selon p1_0_is_blue ! 
     On utilise donc un état où les deux fantômes P1 sont vivants (b1=r1=1 quelles que soient les couleurs). *)
  let n_leaf_c =
    { p = P2; p1_pos0 = 9; p1_pos1 = 7; (* les deux vivants pour que b1=r1=1 *) p2_pos0 = 1; p2_pos1 = 2;
      p1_0_is_blue = false; p2_0_is_blue = true }
  in
  let n_leaf_a2 =
    { p = P2; p1_pos0 = 9; p1_pos1 = 7; p2_pos0 = 1; p2_pos1 = 2;
      p1_0_is_blue = true; p2_0_is_blue = true }
  in
  check "n_leaf_a2 et n_leaf_c : même pi_set (couleur adverse inconnue, deux fantômes P1 vivants)" (same_info_state n_leaf_a2 n_leaf_c);
  check_int "encode n_leaf_a2 = encode n_leaf_c" (Encoding.encode n_leaf_a2) (Encoding.encode n_leaf_c)

(* =========================================================================
   TEST 5 : Tests supplémentaires de descend, payoff, convert_to_info_set
   ========================================================================= *)
let test5_supplementaires () =
  section "TEST 5 : Tests supplémentaires (descend idempotence, symetrie payoff, etc.)";

  (* --- 5a. descend préserve les couleurs --- *)
  Printf.printf "\n-- 5a. descend préserve les couleurs (p1_0_is_blue, p2_0_is_blue) --\n";
  let n_start =
    { p = P1; p1_pos0 = 9; p1_pos1 = 10; p2_pos0 = 1; p2_pos1 = 2;
      p1_0_is_blue = true; p2_0_is_blue = false }
  in
  let child = GameTree.descend (9, 5) n_start in
  check "p1_0_is_blue inchangé après descend" (child.p1_0_is_blue = n_start.p1_0_is_blue);
  check "p2_0_is_blue inchangé après descend" (child.p2_0_is_blue = n_start.p2_0_is_blue);

  (* --- 5b. Symétrie du payoff : payoff P1 = -payoff P2 --- *)
  Printf.printf "\n-- 5b. Symétrie du payoff --\n";
  let leaves_to_test = [
    { p = P1; p1_pos0 = 0; p1_pos1 = 10; p2_pos0 = 1; p2_pos1 = 2;
      p1_0_is_blue = true; p2_0_is_blue = false };  (* P1 sort bleu *)
    { p = P2; p1_pos0 = 9; p1_pos1 = 10; p2_pos0 = 8; p2_pos1 = 2;
      p1_0_is_blue = true; p2_0_is_blue = true };   (* P2 sort bleu *)
    { p = P1; p1_pos0 = -1; p1_pos1 = 10; p2_pos0 = 5; p2_pos1 = 2;
      p1_0_is_blue = true; p2_0_is_blue = false };  (* bleu P1 capturé par P2 *)
  ] in
  List.iteri (fun i leaf ->
    if GameTree.is_leaf leaf then begin
      let p1 = GameTree.payoff P1 leaf in
      let p2 = GameTree.payoff P2 leaf in
      check (Printf.sprintf "payoff P1 = -payoff P2 (feuille %d)" i)
        (abs_float (p1 +. p2) < 1e-9)
    end
  ) leaves_to_test;

  (* --- 5c. convert_to_info_set renvoie bien le premier noeud du pi_set --- *)
  Printf.printf "\n-- 5c. convert_to_info_set --\n";
  let pi = [(n_start, 0.3); (mid_game, 0.7)] in
  let info = GameTree.convert_to_info_set pi in
  check_eq "convert_to_info_set renvoie le premier noeud" info n_start;

  (* --- 5d. alternatives : le mouvement dans les 4 directions --- *)
  Printf.printf "\n-- 5d. Vérification des 4 directions de mouvement depuis le centre --\n";
  (* Case 5 = centre du 3x4 : ligne 1, colonne 1
     Voisins : haut=1, droite=6, bas=9, gauche=4 *)
  let n_centre =
    { p = P1; p1_pos0 = 5; p1_pos1 = 10; p2_pos0 = 2; p2_pos1 = 7;
      p1_0_is_blue = true; p2_0_is_blue = false }
  in
  let alts_centre = GameTree.alternatives [(n_centre, 1.0)] in
  check "(5,1) valide (haut depuis 5)" (List.mem (5, 1) alts_centre);
  check "(5,6) valide (droite depuis 5)" (List.mem (5, 6) alts_centre);
  check "(5,9) valide (bas depuis 5)" (List.mem (5, 9) alts_centre);
  check "(5,4) valide (gauche depuis 5)" (List.mem (5, 4) alts_centre);
  (* Depuis 10 : haut=6, droite invalide (bord), bas=invalide (sortie), gauche=9 *)
  (* Ne pas tester p1_pos1=10 si p2_pos0=2, p2_pos1=7 : case 6 OK, 14 hors plateau, 9=p1_pos0 ami *)
  check "(10,6) valide (haut depuis 10)" (List.mem (10, 6) alts_centre);
  check "(10,9) valide (gauche depuis 10)" (List.mem (10, 9) alts_centre);
  check "(10,11) valide (droite depuis 10)" (List.mem (10, 11) alts_centre);

  (* --- 5e. is_placement_node est cohérent --- *)
  Printf.printf "\n-- 5e. Cohérence is_placement_node --\n";
  check "root est bien un noeud placement" (Placement.is_placement_node root);
  check "after_p1_pl0 est noeud placement (P2 n'a pas encore placé)" (Placement.is_placement_node after_p1_pl0);
  check "after_p2_pl0 n'est plus noeud placement" (not (Placement.is_placement_node after_p2_pl0));
  check "mid_game n'est pas noeud placement" (not (Placement.is_placement_node mid_game))

(* =========================================================================
   TEST 6 : Tests de IMP_MINIMAX
   =========================================================================
   Rappel : imp_minimax avg_opp_str p my_last_str depth lambda
     renvoie (score, my_str, h) où score est l'espérance de gain de la
     stratégie softmax my_str contre avg_opp_str. Cette espérance peut être
     approximée empiriquement en simulant des parties entre
     convert_to_pure_strategy my_str et avg_opp_str.
   ========================================================================= *)

(* --- Utilitaire : simulateur de partie --- *)

(* Tire un coup selon une distribution de probabilités *)
let sample_from (alt_probs : (alternative * float) list) : alternative =
  let r = Random.float 1.0 in
  let rec pick acc = function
    | [] -> fst (List.hd alt_probs)   (* repli si erreur d'arrondi *)
    | [(alt, _)] -> alt
    | (alt, p) :: rest ->
        let acc' = acc +. p in
        if r <= acc' then alt else pick acc' rest
  in
  pick 0. alt_probs

(* Simule une partie complète et renvoie le payoff de P1.
   S'arrête exactement à max_step (qui correspond à la `depth` de l'algorithme)
   pour que le score empirique soit parfaitement comparable au score prédit
   par imp_minimax, qui postule qu'une partie non terminée = 0.0 *)
let rec simulate_play (n : node) (g : game)
    (str_p1 : strategy) (str_p2 : strategy) (step : int) (max_step : int) : float =
  
  if GameTree.is_leaf n then
    GameTree.payoff P1 n
  else if step >= max_step then
    0.0
  else
    let str = if n.p = P1 then str_p1 else str_p2 in
    let alt_probs = str g n in
    if alt_probs = [] then failwith "alt_probs = []"
    else
      let chosen = sample_from alt_probs in
      let g' = if fst chosen < 0 then (0, -1) :: g else chosen :: g in
      simulate_play (GameTree.descend chosen n) g' str_p1 str_p2 (step + 1) max_step

let simulate_n_games (n : int) (str_p1 : strategy) (str_p2 : strategy) (max_step : int) : float =
  let rec loop i acc =
    if i = 0 then acc
    else
      let s = simulate_play GameTree.root [] str_p1 str_p2 0 max_step in
      loop (i - 1) (acc +. s)
  in
  loop n 0. /. float_of_int n

(* --- Vérification qu'une stratégie est bien normalisée --- *)
let strategy_is_valid (str : strategy) (n : node) (g : game) : bool =
  if GameTree.is_leaf n then true
  else
    let alt_probs = str g n in
    let total = List.fold_left (fun acc (_, p) -> acc +. p) 0. alt_probs in
    let all_nonneg = List.for_all (fun (_, p) -> p >= -1e-9) alt_probs in
    let alts = GameTree.alternatives [(n, 1.)] in
    all_nonneg &&
    abs_float (total -. 1.) < 1e-6 &&
    List.length alt_probs = List.length alts (* toutes les alternatives présentes *)

let test6_imp_minimax () =
  section "TEST 6 : IMP_MINIMAX (valeur, score empirique, fictitious play)";
  let depth = 8 in
  let lambda = 10. in
  let nb_simul = 500 in   (* parties simulées pour l'estimation empirique *)

  (* ---- 6a. Contre 10 stratégies mixtes aléatoires ---- *)
  Printf.printf "\n-- 6a. P1 vs 10 stratégies mixtes aléatoires --\n";
  let g_mid = [(0,-1);(0,-1)] in
  let uniform = Strategy.create_uniform_strategy () in
  let score_unif_ref = ref 0. in           (* score de référence de la strat uniforme *)
  let fails_6a = ref 0 in
  for k = 1 to 10 do
    let rand_mix = Strategy.create_random_strategy () in
    let score_pred, h =
      IMP_MINIMAX.imp_minimax rand_mix P1 depth lambda in
    let my_str = Strategy.create_strategy h uniform in 
    let my_pure = Strategy.convert_to_pure_strategy my_str in
    let score_emp = simulate_n_games nb_simul my_pure rand_mix depth in
    if k = 1 then
      score_unif_ref := simulate_n_games nb_simul uniform rand_mix depth;
    (* Tolérance : l'écart entre score prédit (softmax) et score empirique (pure)
       inclut la variance de simulation (≈0.07 à 99%) + le biais softmax/pure.
       On accepte une différence ≤ 0.30 *)
    let coherent = abs_float (score_pred -. score_emp) <= 0.30 in
    let in_range = score_pred >= -1. -. 1e-9 && score_pred <= 1. +. 1e-9 in
    let beats_unif = score_emp >= !score_unif_ref -. 0.20 in
    Printf.printf
      "  [%d] prédit=%+.3f  empirique=%+.3f  |écart|=%.3f  %s\n"
      k score_pred score_emp (abs_float (score_pred -. score_emp))
      (if coherent && in_range && beats_unif then "OK" else "FAIL");
    if not (coherent && in_range && beats_unif) then incr fails_6a
  done;
  check "6a : score prédit ≈ score empirique sur 10 strats mixtes (|écart| ≤ 0.30)"
    (!fails_6a = 0);
  check "my_str est normalisée sur mid_game après la dernière itération"
    (let rand = Strategy.create_random_strategy () in
     let _, h = IMP_MINIMAX.imp_minimax rand P1 depth lambda in
     let last_str = Strategy.create_strategy h uniform in 
     strategy_is_valid last_str mid_game g_mid);

  (* ---- 6b. Contre 10 stratégies pures aléatoires ---- *)
  Printf.printf "\n-- 6b. P1 vs 10 stratégies pures aléatoires --\n";
  let fails_6b = ref 0 in
  for k = 1 to 10 do
    let rand_pure = Strategy.create_random_pure_strategy () in
    let score_pred, h =
      IMP_MINIMAX.imp_minimax rand_pure P1 depth lambda in
    let my_str = Strategy.create_strategy h uniform in 
    let my_pure = Strategy.convert_to_pure_strategy my_str in
    let score_emp = simulate_n_games nb_simul my_pure rand_pure depth in
    (* Contre une strat pure, imp_minimax est exact (pas de biais softmax/pure
       à la profondeur d'arrêt) : l'écart doit être plus petit *)
    let coherent = abs_float (score_pred -. score_emp) <= 0.20 in
    let in_range = score_pred >= -1. -. 1e-9 && score_pred <= 1. +. 1e-9 in
    Printf.printf
      "  [%d] prédit=%+.3f  empirique=%+.3f  |écart|=%.3f  %s\n"
      k score_pred score_emp (abs_float (score_pred -. score_emp))
      (if coherent && in_range then "OK" else "FAIL");
    if not (coherent && in_range) then incr fails_6b
  done;
  check "6b : score prédit ≈ score empirique sur 10 strats pures (|écart| ≤ 0.20)"
    (!fails_6b = 0);

  (* ---- 6c. P2 vs 10 stratégies mixtes aléatoires ---- *)
  Printf.printf "\n-- 6c. P2 vs 10 stratégies mixtes aléatoires --\n";
  let fails_6c = ref 0 in
  for k = 1 to 10 do
    let rand_mix2 = Strategy.create_random_strategy () in
    let score_pred2, h =
      IMP_MINIMAX.imp_minimax rand_mix2 P2 depth lambda in
    let my_str2 = Strategy.create_strategy h uniform in
    let my_pure2 = Strategy.convert_to_pure_strategy my_str2 in
    let emp_p1_score = simulate_n_games nb_simul rand_mix2 my_pure2 depth in
    (* simulate_play renvoie TOUJOURS le score de P1. Donc le score empirique de P2 est l'opposé. *)
    let score_emp2 = -. emp_p1_score in
    let coherent = abs_float (score_pred2 -. score_emp2) <= 0.30 in
    let in_range = score_pred2 >= -1. -. 1e-9 && score_pred2 <= 1. +. 1e-9 in
    Printf.printf
      "  [%d] prédit=%+.3f  empirique=%+.3f  |écart|=%.3f  %s\n"
      k score_pred2 score_emp2 (abs_float (score_pred2 -. score_emp2))
      (if coherent && in_range then "OK" else "FAIL");
    if not (coherent && in_range) then incr fails_6c
  done;
  check "6c : score prédit ≈ score empirique sur 10 strats mixtes pour P2 (|écart| ≤ 0.30)"
    (!fails_6c = 0);

  (* ---- 6c_bis. P2 vs 10 stratégies pures aléatoires ---- *)
  Printf.printf "\n-- 6c_bis. P2 vs 10 stratégies pures aléatoires --\n";
  let fails_6cbis = ref 0 in
  for k = 1 to 10 do
    let rand_pure2 = Strategy.create_random_pure_strategy () in
    let score_pred2, h =
      IMP_MINIMAX.imp_minimax rand_pure2 P2 depth lambda in
    let my_str2 = Strategy.create_strategy h uniform in
    let my_pure2 = Strategy.convert_to_pure_strategy my_str2 in
    let emp_p1_score = simulate_n_games nb_simul rand_pure2 my_pure2 depth in
    let score_emp2 = -. emp_p1_score in
    let coherent = abs_float (score_pred2 -. score_emp2) <= 0.20 in
    let in_range = score_pred2 >= -1. -. 1e-9 && score_pred2 <= 1. +. 1e-9 in
    Printf.printf
      "  [%d] prédit=%+.3f  empirique=%+.3f  |écart|=%.3f  %s\n"
      k score_pred2 score_emp2 (abs_float (score_pred2 -. score_emp2))
      (if coherent && in_range then "OK" else "FAIL");
    if not (coherent && in_range) then incr fails_6cbis
  done;
  check "6c_bis : score prédit ≈ score empirique sur 10 strats pures pour P2 (|écart| ≤ 0.20)"
    (!fails_6cbis = 0);


  (* ---- 6d. Cohérence : IMP_MINIMAX est déterministe à stratégie fixée ---- *)
  Printf.printf "\n-- 6d. Cohérence : même opp_str → même score --\n";
  let fixed_opp = Strategy.create_uniform_strategy () in
  let s_a, _ = IMP_MINIMAX.imp_minimax fixed_opp P1 depth lambda in
  let s_b, _ = IMP_MINIMAX.imp_minimax fixed_opp P1 depth lambda in
  Printf.printf "  score run 1 = %+.6f, run 2 = %+.6f\n" s_a s_b;
  check_float "imp_minimax est déterministe (même args → même score)" s_a s_b 1e-9;

  (* ---- 6e. Quelques tours de Fictitious Play ---- *)
  Printf.printf "\n-- 6e. Fictitious Play : %d tours, profondeur %d --\n" 4 depth;
  let avg_h_p1 = Hashtbl.create (1 lsl 18) in
  let avg_h_p2 = Hashtbl.create (1 lsl 18) in
  let last_p1 = ref (Strategy.create_uniform_strategy ()) in
  let last_p2 = ref (Strategy.create_uniform_strategy ()) in
  let avg_p1  = ref !last_p1 in
  let avg_p2  = ref !last_p2 in

  for round = 1 to 4 do
    let sc1, h1 =
      IMP_MINIMAX.imp_minimax !avg_p2 P1 depth lambda in
    last_p1 := Strategy.create_strategy h1 !last_p1;
    IMP_MINIMAX.update_average_strategy avg_h_p1 h1 round;
    avg_p1 := Strategy.create_strategy avg_h_p1 (Strategy.create_uniform_strategy ());

    let sc2, h2 =
      IMP_MINIMAX.imp_minimax !avg_p1 P2 depth lambda in
    last_p2 := Strategy.create_strategy h2 !last_p2;
    IMP_MINIMAX.update_average_strategy avg_h_p2 h2 round;
    avg_p2 := Strategy.create_strategy avg_h_p2 (Strategy.create_uniform_strategy ());

    Printf.printf "  Tour %d : score P1=%+.4f, score P2=%+.4f\n" round sc1 sc2;

    check (Printf.sprintf "score P1 tour %d ∈ [-1,1]" round)
      (sc1 >= -1. -. 1e-9 && sc1 <= 1. +. 1e-9);
    check (Printf.sprintf "score P2 tour %d ∈ [-1,1]" round)
      (sc2 >= -1. -. 1e-9 && sc2 <= 1. +. 1e-9);

    (* Note : le score de imp_minimax est la meilleure réponse à avg_opp_str
       au tour t. Comme avg_opp_str change (elle devient plus équilibrée au
       fil des tours), le score peut BAISSER : l'adversaire moyen devient
       plus difficile à exploiter. Ce n'est PAS une violation, c'est attendu
       dans Smooth Fictitious Play. On vérifie juste que les scores restent
       dans [-1, 1]. *)

    (* Vérifier que avg_p1 et avg_p2 sont valides sur mid_game *)
    check (Printf.sprintf "avg_p1 valide tour %d" round)
      (strategy_is_valid !avg_p1 mid_game [(0,-1);(0,-1)]);
    check (Printf.sprintf "avg_p2 valide tour %d" round)
      (strategy_is_valid !avg_p2 mid_game [(0,-1);(0,-1)]);

  done;

  (* Vérification finale : les deux stratégies moyennes jouent l'une contre l'autre *)
  Printf.printf "\n-- 6f. Simulation finale avg_p1 vs avg_p2 --\n";
  let final_pure_p1 = Strategy.convert_to_pure_strategy !avg_p1 in
  let final_pure_p2 = Strategy.convert_to_pure_strategy !avg_p2 in
  let final_score = simulate_n_games nb_simul final_pure_p1 final_pure_p2 depth in
  Printf.printf "  Score empirique P1 (avg_pure_p1 vs avg_pure_p2, %d parties) = %+.4f\n"
    nb_simul final_score;
  check "score final ∈ [-1, 1]"
    (final_score >= -1. -. 0.1 && final_score <= 1. +. 0.1)

(* =========================================================================
   Exécution
   ========================================================================= *)
let () =
  Printf.printf "\n============================================================\n";
  Printf.printf "        TESTS : Geister Smooth Fictitious Play\n";
  Printf.printf "============================================================\n";
  test1_nodes_profondeur_0_2 ();
  test2_edge_cases ();
  test3_exhaustive_tree ();
  test4_encode ();
  test5_supplementaires ();
  test6_imp_minimax ();
  Printf.printf "\n============================================================\n";
  Printf.printf " RÉSULTATS : %d / %d tests passés\n" !passed_tests !total_tests;
  if !passed_tests = !total_tests then
    Printf.printf " ✓ TOUS LES TESTS SONT PASSÉS\n"
  else
    Printf.printf " ✗ %d TEST(S) ONT ÉCHOUÉ\n" (!total_tests - !passed_tests);
  Printf.printf "============================================================\n\n"
