(* ==========================================================================
   Point d'entrée principal — menu interactif en ligne de commande
   ==========================================================================
   Compile avec :
    ocamlfind ocamlopt -linkpkg -package dynlink geister_smooth_fictitious_play.ml main.ml -o geister && ./geister

   ========================================================================== *)

open Geister_smooth_fictitious_play


(* --------------------------------------------------------------------------
   Utilitaire : demander un joueur (P1 / P2) à l'utilisateur
   -------------------------------------------------------------------------- *)
let ask_player () : player =
    Printf.printf "Quel joueur êtes-vous ? (1 = P1, 2 = P2) : ";
    flush stdout;
    let n = try read_int () with Failure _ -> 0 in
    match n with
    | 1 -> P1
    | 2 -> P2
    | _ -> Printf.printf "Choix invalide, P1 par défaut.\n"; P1


(* --------------------------------------------------------------------------
   Utilitaire : demander un entier
   -------------------------------------------------------------------------- *)
let ask_int (prompt : string) : int =
    Printf.printf "%s" prompt;
    flush stdout;
    try read_int ()
    with Failure _ ->
        Printf.printf "Entrée invalide.\n";
        exit 1


(* --------------------------------------------------------------------------
   Utilitaire : demander un flottant
   -------------------------------------------------------------------------- *)
let ask_float (prompt : string) : float =
    Printf.printf "%s" prompt;
    flush stdout;
    try read_float ()
    with Failure _ ->
        Printf.printf "Entrée invalide.\n";
        exit 1


(* --------------------------------------------------------------------------
   Utilitaire : extraire le tour depuis le nom du fichier (ou demander)
   -------------------------------------------------------------------------- *)
let extract_round_or_ask (fname : string) : int =
  let basename = Filename.basename fname in
  let rec extract_t_num i =
    if i >= String.length basename then None
    else if basename.[i] = 't' then
        let rec get_num j acc =
            if j < String.length basename && basename.[j] >= '0' && basename.[j] <= '9' then
                get_num (j + 1) (acc ^ String.make 1 basename.[j])
            else if acc = "" then None else Some (int_of_string acc)
        in
        match get_num (i + 1) "" with
        | Some n -> Some n
        | None -> extract_t_num (i + 1)
    else extract_t_num (i + 1)
  in
  match extract_t_num 0 with
  | Some r -> 
      Printf.printf "Tour de reprise détecté automatiquement : %d\n" r;
      r
  | None ->
      Printf.printf "Impossible de détecter le tour à partir du nom du fichier.\n";
      ask_int "A partir de quel tour souhaitez-vous reprendre ? (ex: si le fichier s'est arrêté au tour 150, reprenez de 150) : "


(* --------------------------------------------------------------------------
   Utilitaire : demander un layout d'évolution lambda
   -------------------------------------------------------------------------- *)
let ask_schedule () : lambda_schedule =
    Printf.printf "\nChoix de la progression de Lambda (Équilibre de Nash) :\n";
    Printf.printf "  1. Constant       (lambda_t = C)\n";
    Printf.printf "  2. Linéaire       (lambda_t = l_0 + (l_max - l_0) * t/N)\n";
    Printf.printf "  3. Racine Carrée  (lambda_t = c * sqrt(t))\n";
    Printf.printf "  4. Exponentielle  (lambda_t = l_0 * alpha^t)\n";
    let choice = ask_int "Votre choix (1-4) : " in
    match choice with
    | 1 -> Constant (ask_float "Valeur constante de Lambda (ex: 0.1) : ")
    | 2 -> 
        let l0 = ask_float "Lambda initial (ex: 0.01) : " in
        let lmax = ask_float "Lambda final (ex: 1.0) : " in
        Linear (l0, lmax)
    | 3 -> Sqrt (ask_float "Facteur de croissance 'c' (ex: 0.05) : ")
    | 4 ->
        let l0 = ask_float "Lambda initial (ex: 0.01) : " in
        let alpha = ask_float "Alpha multiplicateur (ex: 1.05) : " in
        Exponential (l0, alpha)
    | _ -> Printf.printf "Choix invalide, par défaut Constant(0.1).\n"; Constant 0.1



(* --------------------------------------------------------------------------
   Utilitaire : demander un fichier
   -------------------------------------------------------------------------- *)
let ask_file (prompt : string) : string =
    Printf.printf "%s" prompt;
    flush stdout;
    let f = read_line () in
    if not (Sys.file_exists f) then begin
        Printf.printf "Fichier '%s' introuvable.\n" f;
        exit 1
    end;
    f


(* --------------------------------------------------------------------------
   Option 1 : Jouer contre une stratégie
   -------------------------------------------------------------------------- *)
let mode_play_against_strategy () =
    Printf.printf "\n=== Jouer contre une stratégie ===\n\n";
    let ml_file = ask_file "Chemin du fichier stratégie (.ml) : " in
    let user_player = ask_player () in
    let strat = StrategyIO.load_strategy_from_file ml_file in
    Printf.printf "\nDébut de la partie ! Vous jouez %s.\n\n"
        (match user_player with P1 -> "P1" | P2 -> "P2");
    UserInterface.user_against_strategy strat user_player


(* --------------------------------------------------------------------------
   Option 2 : Faire s'affronter deux stratégies
   -------------------------------------------------------------------------- *)
let mode_strategy_vs_strategy () =
    Printf.printf "\n=== Affrontement de deux stratégies ===\n\n";
    let ml_p1 = ask_file "Chemin du fichier stratégie P1 (.ml) : " in
    let ml_p2 = ask_file "Chemin du fichier stratégie P2 (.ml) : " in
    let nb_games = ask_int "Nombre de parties : " in
    let depth = ask_int "Profondeur maximale (nombre de coups total) : " in
    let strat_p1 = StrategyIO.load_strategy_from_file ml_p1 in
    let strat_p2 = if ml_p1 <> ml_p2 then StrategyIO.load_strategy_from_file ml_p2 else strat_p1 in
    Printf.printf "\nSimulation de %d parties (profondeur %d)...\n" nb_games depth;
    let score = UserInterface.test_strategies strat_p1 strat_p2 nb_games depth in
    Printf.printf "\nScore moyen de P1 : %+.4f\n" score;
    Printf.printf "(+1 = P1 gagne toujours, -1 = P2 gagne toujours, 0 = égalité)\n"


(* --------------------------------------------------------------------------
   Option 3 : Smooth Fictitious Play — nombre de tours limité
   -------------------------------------------------------------------------- *)
let mode_sfp_rounds () =
    Printf.printf "\n=== Smooth Fictitious Play (tours limités) ===\n\n";
    let nb_rounds = ask_int "Nombre de tours : " in
    let depth = ask_int "Profondeur : " in
    let sched = ask_schedule () in
    let ml_p1 = ask_file "Chemin du fichier stratégie initiale P1 (.ml) : " in
    let ml_p2 = ask_file "Chemin du fichier stratégie initiale P2 (.ml) : " in
    Printf.printf "\nLancement du Smooth Fictitious Play (%d tours, profondeur %d)...\n\n"
        nb_rounds depth;
    IMP_MINIMAX.smooth_fictitious_play (ByRounds nb_rounds) depth sched ml_p1 ml_p2


(* --------------------------------------------------------------------------
   Option 4 : Smooth Fictitious Play — temps limité
   -------------------------------------------------------------------------- *)
let mode_sfp_time () =
    Printf.printf "\n=== Smooth Fictitious Play (temps limité) ===\n\n";
    let time = ask_float "Temps imparti (en secondes) : " in
    let depth = ask_int "Profondeur : " in
    let sched = ask_schedule () in
    let ml_p1 = ask_file "Chemin du fichier stratégie initiale P1 (.ml) : " in
    let ml_p2 = ask_file "Chemin du fichier stratégie initiale P2 (.ml) : " in
    Printf.printf "\nLancement du Smooth Fictitious Play (%.1fs, profondeur %d)...\n\n"
        time depth;
    IMP_MINIMAX.smooth_fictitious_play (ByTime time) depth sched ml_p1 ml_p2


(* --------------------------------------------------------------------------
   Option 5 / 6 : stratégie heuristique comme stratégie initiale
   -------------------------------------------------------------------------- *)
let mode_sfp_rounds_heuristic () =
    Printf.printf "\n=== Smooth Fictitious Play (tours, strats initiales heuristiques) ===\n\n";
    let nb_rounds = ask_int "Nombre de tours : " in
    let depth = ask_int "Profondeur : " in
    let sched = ask_schedule () in
    let heuristic_path = "strategies/heuristic.ml" in
    Printf.printf "\nLancement du Smooth Fictitious Play (%d tours, profondeur %d)...\n\n"
        nb_rounds depth;
    IMP_MINIMAX.smooth_fictitious_play (ByRounds nb_rounds) depth sched heuristic_path heuristic_path


let mode_sfp_time_heuristic () =
    Printf.printf "\n=== Smooth Fictitious Play (temps, strats initiales heuristiques) ===\n\n";
    let time = ask_float "Temps imparti (en secondes) : " in
    let depth = ask_int "Profondeur : " in
    let sched = ask_schedule () in
    let heuristic_path = "strategies/heuristic.ml" in
    Printf.printf "\nLancement du Smooth Fictitious Play (%.1fs, profondeur %d)...\n\n"
        time depth;
    IMP_MINIMAX.smooth_fictitious_play (ByTime time) depth sched heuristic_path heuristic_path


(* --------------------------------------------------------------------------
   Option 7 / 8 : Reprendre une recherche SFP
   -------------------------------------------------------------------------- *)
let mode_sfp_rounds_resume () =
    Printf.printf "\n=== Smooth Fictitious Play (reprendre, tours additionnels) ===\n\n";
    let dat_p1 = ask_file "Chemin du fichier de sauvegarde P1 (.dat) : " in
    let dat_p2 = ask_file "Chemin du fichier de sauvegarde P2 (.dat) : " in
    let r0 = extract_round_or_ask dat_p1 in
    let extra_rounds = ask_int "Nombre de tours SUPPLEMENTAIRES à effectuer : " in
    let depth = ask_int "Profondeur : " in
    let sched = ask_schedule () in
    let ml_p1 = ask_file "Chemin du fichier de stratégie de secours P1 (.ml) (ex: strategies/heuristic.ml): " in
    let ml_p2 = ask_file "Chemin du fichier de stratégie de secours P2 (.ml) : " in
    Printf.printf "\nReprise du SFP au tour %d (pour %d nouveaux tours, profondeur %d)...\n\n"
        (r0 + 1) extra_rounds depth;
    IMP_MINIMAX.smooth_fictitious_play ~resume:(Some (dat_p1, dat_p2, r0)) (ByRounds extra_rounds) depth sched ml_p1 ml_p2

let mode_sfp_time_resume () =
    Printf.printf "\n=== Smooth Fictitious Play (reprendre, temps additionnel) ===\n\n";
    let dat_p1 = ask_file "Chemin du fichier de sauvegarde P1 (.dat) : " in
    let dat_p2 = ask_file "Chemin du fichier de sauvegarde P2 (.dat) : " in
    let r0 = extract_round_or_ask dat_p1 in
    let time = ask_float "Temps SUPPLEMENTAIRE imparti (en secondes) : " in
    let depth = ask_int "Profondeur : " in
    let sched = ask_schedule () in
    let ml_p1 = ask_file "Chemin du fichier de stratégie de secours P1 (.ml) (ex: strategies/heuristic.ml): " in
    let ml_p2 = ask_file "Chemin du fichier de stratégie de secours P2 (.ml) : " in
    Printf.printf "\nReprise du SFP au tour %d (pour %.1fs supplémentaires, profondeur %d)...\n\n"
        (r0 + 1) time depth;
    IMP_MINIMAX.smooth_fictitious_play ~resume:(Some (dat_p1, dat_p2, r0)) (ByTime time) depth sched ml_p1 ml_p2



(* ==========================================================================
   Menu principal
   ========================================================================== *)
let () =
    Printf.printf "\n";
    Printf.printf "╔═════════════════════════════════════════════════════════╗\n";
    Printf.printf "║         Geister — Smooth Fictitious Play                ║\n";
    Printf.printf "║         BELLIOT Raphaël — TIPE 2024-2025                ║\n";
    Printf.printf "╠═════════════════════════════════════════════════════════╣\n";
    Printf.printf "║                                                         ║\n";
    Printf.printf "║  1. Jouer contre une stratégie                          ║\n";
    Printf.printf "║  2. Affronter deux stratégies entre elles               ║\n";
    Printf.printf "║  3. SFP — tours limités (strats initiales fichier)      ║\n";
    Printf.printf "║  4. SFP — temps limité  (strats initiales fichier)      ║\n";
    Printf.printf "║  5. SFP — tours limités (strats initiales heuristiques) ║\n";
    Printf.printf "║  6. SFP — temps limité  (strats initiales heuristiques) ║\n";
    Printf.printf "║  7. SFP — REPRENDRE une recherche (tours additionnels)  ║\n";
    Printf.printf "║  8. SFP — REPRENDRE une recherche (temps additionnel)   ║\n";
    Printf.printf "║                                                         ║\n";
    Printf.printf "╚═════════════════════════════════════════════════════════╝\n";
    Printf.printf "\n";
    let choice = ask_int "Votre choix (1-8) : " in
    match choice with
    | 1 -> mode_play_against_strategy ()
    | 2 -> mode_strategy_vs_strategy ()
    | 3 -> mode_sfp_rounds ()
    | 4 -> mode_sfp_time ()
    | 5 -> mode_sfp_rounds_heuristic ()
    | 6 -> mode_sfp_time_heuristic ()
    | 7 -> mode_sfp_rounds_resume ()
    | 8 -> mode_sfp_time_resume ()
    | _ -> Printf.printf "Choix invalide. Veuillez relancer le programme.\n"
