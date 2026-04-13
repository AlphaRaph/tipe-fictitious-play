(* ==========================================================================
   Stratégie : Heuristique
   Description : Stratégie évaluative puissante exploitant les rôles asymétriques
   entre fantômes bleus et rouges.

   MÉCANIQUE DE L'HEURISTIQUE :
   1. Fantômes Bleus (Coureurs) : L'heuristique donne un très fort bonus à leur
      rapprochement (distance de Manhattan) de la case de sortie adverse la
      plus proche. Un malus massif est appliqué s'ils sont adjacents à un
      adversaire, incitant à une fuite précautionneuse. Si le bleu meurt, la 
      pénalité est énorme (game over potentiel).
   2. Fantômes Rouges (Chasseurs/Kamikazes) : L'heuristique récompense le fait 
      de se rapprocher des fantômes adverses ou de bloquer les sorties adverses.
      Le but est d'empoisonner l'adversaire ou de bloquer le chemin de ses 
      bleus potentiels.
   3. Défense : Un fort malus est appliqué si les fantômes adverses se rapprochent
      de nos propres sorties. Cela force naturellement nos fantômes (via les
      rouges en priorité) à se positionner en bouclier.
   4. Placements (Nash Equilibrium) : Au tout premier tour pour poser ses fantômes,
      les différentes dispositions sont choisies de manière parfaitement équiprobable
      (uniforme) pour ne donner aucune faille à exploiter à l'adversaire.
   5. Probabilités Softmax : Plutôt que de jouer le 100% de la meilleure option, 
      le calcul convertit les scores en probabilités pondérées (Température/Lambda)
      ce qui confère de l'imprévisibilité et évite l'exploitabilité.
   ========================================================================== *)

open Geister_smooth_fictitious_play

(* Distance de Manhattan adaptée au plateau *)
let dist pos1 pos2 = 
  if pos1 < 0 || pos2 < 0 then 100 (* Loin du plateau (mangé ou non positionné) *)
  else
    let c1 = pos1 mod Config.board_width in
    let r1 = pos1 / Config.board_width in
    let c2 = pos2 mod Config.board_width in
    let r2 = pos2 / Config.board_width in
    abs (c1 - c2) + abs (r1 - r2)

(* Fonction d'évaluation principale, valant pour 'me' après un potentiel coup *)
let eval (n: node) (me: player) : float =
  if GameTree.is_leaf n then
    let p = GameTree.payoff me n in
    p *. 10000.0   (* Victoire/Défaite absolue *)
  else
    (* Identification des pièces alliées et adverses *)
    let (our_b, our_r, our_exits, opp_pos0, opp_pos1, opp_exits) = 
      match me with
      | P1 -> 
          let b, r = if n.p1_0_is_blue then (n.p1_pos0, n.p1_pos1) else (n.p1_pos1, n.p1_pos0) in
          (b, r, Config.p1_exit_cases, n.p2_pos0, n.p2_pos1, Config.p2_exit_cases)
      | P2 ->
          let b, r = if n.p2_0_is_blue then (n.p2_pos0, n.p2_pos1) else (n.p2_pos1, n.p2_pos0) in
          (b, r, Config.p2_exit_cases, n.p1_pos0, n.p1_pos1, Config.p1_exit_cases)
    in
    
    let score = ref 0.0 in
    let active_opps = List.filter (fun x -> x >= 0) [opp_pos0; opp_pos1] in
    
    (* ==================== RÔLE BLEU (Course & Survie) ==================== *)
    if our_b >= 0 then begin
      (* Progression vers la sortie *)
      let d_exit = List.fold_left (fun acc e -> min acc (dist our_b e)) 100 our_exits in
      score := !score +. 100.0 -. (float_of_int d_exit *. 15.0);
      
      (* Instinct de survie (fuir l'adversaire) *)
      let min_d_opp = List.fold_left (fun acc o -> min acc (dist our_b o)) 100 active_opps in
      if min_d_opp = 1 then
        score := !score -. 80.0 (* Danger imminent *)
      else if min_d_opp = 2 then
        score := !score -. 30.0 (* Danger immédiat *)
    end else begin
      (* Si notre fantôme bleu est mort, c'est désastreux *)
      score := !score -. 1000.0
    end;
    
    (* ==================== RÔLE ROUGE (Chasse & Blocage) ==================== *)
    if our_r >= 0 then begin
      (* Chercher le contact *)
      let min_d_opp = List.fold_left (fun acc o -> min acc (dist our_r o)) 100 active_opps in
      if min_d_opp < 100 then
        score := !score +. 30.0 -. (float_of_int min_d_opp *. 8.0);
        
      (* Bloquer les sorties de l'adversaire (qui sont 'opp_exits') *)
      let d_block = List.fold_left (fun acc e -> min acc (dist our_r e)) 100 opp_exits in
      score := !score +. 20.0 -. (float_of_int d_block *. 4.0)
    end;
    
    (* ==================== DÉFENSE (Empêcher adverse d'atteindre SES sorties) ==================== *)
    List.iter (fun opp ->
       let d_exit_opp = List.fold_left (fun acc e -> min acc (dist opp e)) 100 opp_exits in
       score := !score -. (100.0 -. float_of_int d_exit_opp *. 20.0)
    ) active_opps;
    
    !score

(* Transforme les scores (valeurs hétérogènes) en réseau de probabilités : 
   Avantage massivement le meilleur score tout en ne tombant pas à 0 
   totalement pour les moins bons. *)
let softmax alt_scores lambda =
  if alt_scores = [] then [] else
  let max_score = List.fold_left (fun acc (_, s) -> max acc s) neg_infinity alt_scores in
  let exp_scores = List.map (fun (a, s) -> (a, exp (lambda *. (s -. max_score)))) alt_scores in
  let sum_exp = List.fold_left (fun acc (_, es) -> acc +. es) 0.0 exp_scores in
  List.map (fun (a, es) -> (a, es /. sum_exp)) exp_scores

(* Construit la stratégie à partir des évaluations Softmax *)
let alt_probs (g : game) (i : info_set) : (alternative * float) list =
  let alts = GameTree.alternatives [(i, 1.0)] in
  if alts = [] then []
  else if fst (List.hd alts) < 0 then
    (* Étape 1 : le placement n'obéit pas à l'heuristique,
       il doit impérativement être aléatoire et parfaitement uniforme.*)
    let n = List.length alts in
    let w = 1.0 /. float_of_int n in
    List.map (fun alt -> (alt, w)) alts
  else
    (* Étape 2 : en cours de jeu, évaluation du coup futur simulé. *)
    let me = i.p in
    let alt_scores = List.map (fun alt ->
       let next_state = GameTree.descend alt i in
       let score = eval next_state me in
       (alt, score)
    ) alts in
    
    (* Paramètre Lambda à ajuster (0.15 = balance conservatrice mais bruitée) *)
    softmax alt_scores 0.15

(** Enregistrement pour le chargement dynamique **)
let () = StrategyRegistry.register alt_probs
