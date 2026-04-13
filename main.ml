(* ==========================================================================
   Point d'entrée principal — menu interactif en ligne de commande
   ==========================================================================
   Compile avec :
    ocamlfind ocamlopt -linkpkg -package dynlink geister_smooth_fictitious_play.ml main.ml -o geister && ./geister

   ========================================================================== *)

open Geister_smooth_fictitious_play


(* --------------------------------------------------------------------------
   Utilitaire : charger une stratégie depuis un fichier .ml
   -------------------------------------------------------------------------- *)
let load_strategy (ml_file : string) : strategy =
    let cmxs_file = Filename.chop_extension ml_file ^ ".cmxs" in
    (* Compilation du fichier .ml en bibliothèque partagée .cmxs *)
    (* On ne lie PAS geister_smooth_fictitious_play.cmx ici car il est déjà chargé par le programme principal. *)
    (* Dynlink résoudra les références au chargement. *)
    let cmd = Printf.sprintf "ocamlopt -shared -I . -open Geister_smooth_fictitious_play %s -o %s" ml_file cmxs_file in
    if Sys.command cmd <> 0 then begin
        Printf.printf "Erreur : échec de la compilation de '%s' en .cmxs.\n" ml_file;
        exit 1
    end;

    (* Chargement dynamique du module compilé *)
    (try Dynlink.loadfile cmxs_file with
    | Dynlink.Error e ->
        Printf.printf "Erreur : échec du chargement de '%s' : %s\n" cmxs_file (Dynlink.error_message e);
        exit 1);

    (* Récupération de la stratégie via le registre *)
    StrategyRegistry.get_and_clear ()


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
    let strat = load_strategy ml_file in
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
    let strat_p1 = load_strategy ml_p1 in
    let strat_p2 = if ml_p1 <> ml_p2 then load_strategy ml_p2 else strat_p1 in
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
    let init_p1 = load_strategy ml_p1 in
    let init_p2 = if ml_p1 <> ml_p2 then load_strategy ml_p2 else init_p1 in
    Printf.printf "\nLancement du Smooth Fictitious Play (%d tours, profondeur %d)...\n\n"
        nb_rounds depth;
    IMP_MINIMAX.smooth_fictitious_play nb_rounds depth sched init_p1 init_p2


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
    let init_p1 = load_strategy ml_p1 in
    let init_p2 = if ml_p1 <> ml_p2 then load_strategy ml_p2 else init_p1 in
    Printf.printf "\nLancement du Smooth Fictitious Play (%.1fs, profondeur %d)...\n\n"
        time depth;
    IMP_MINIMAX.smooth_fictitious_play_with_time time depth sched init_p1 init_p2


(* --------------------------------------------------------------------------
   Option 5 (bonus) : stratégie uniforme comme stratégie initiale
   -------------------------------------------------------------------------- *)
let mode_sfp_rounds_uniform () =
    Printf.printf "\n=== Smooth Fictitious Play (tours, strats initiales uniformes) ===\n\n";
    let nb_rounds = ask_int "Nombre de tours : " in
    let depth = ask_int "Profondeur : " in
    let sched = ask_schedule () in
    let init_p1 = Strategy.create_uniform_strategy () in
    let init_p2 = Strategy.create_uniform_strategy () in
    Printf.printf "\nLancement du Smooth Fictitious Play (%d tours, profondeur %d)...\n\n"
        nb_rounds depth;
    IMP_MINIMAX.smooth_fictitious_play nb_rounds depth sched init_p1 init_p2


let mode_sfp_time_uniform () =
    Printf.printf "\n=== Smooth Fictitious Play (temps, strats initiales uniformes) ===\n\n";
    let time = ask_float "Temps imparti (en secondes) : " in
    let depth = ask_int "Profondeur : " in
    let sched = ask_schedule () in
    let init_p1 = Strategy.create_uniform_strategy () in
    let init_p2 = Strategy.create_uniform_strategy () in
    Printf.printf "\nLancement du Smooth Fictitious Play (%.1fs, profondeur %d)...\n\n"
        time depth;
    IMP_MINIMAX.smooth_fictitious_play_with_time time depth sched init_p1 init_p2


(* ==========================================================================
   Menu principal
   ========================================================================== *)
let () =
    Printf.printf "\n";
    Printf.printf "╔════════════════════════════════════════════════════════╗\n";
    Printf.printf "║        Geister — Smooth Fictitious Play                ║\n";
    Printf.printf "║        BELLIOT Raphaël — TIPE 2024-2025                ║\n";
    Printf.printf "╠════════════════════════════════════════════════════════╣\n";
    Printf.printf "║                                                        ║\n";
    Printf.printf "║  1. Jouer contre une stratégie                         ║\n";
    Printf.printf "║  2. Affronter deux stratégies entre elles              ║\n";
    Printf.printf "║  3. SFP — tours limités (strats initiales fichier)     ║\n";
    Printf.printf "║  4. SFP — temps limité  (strats initiales fichier)     ║\n";
    Printf.printf "║  5. SFP — tours limités (strats initiales uniformes)   ║\n";
    Printf.printf "║  6. SFP — temps limité  (strats initiales uniformes)   ║\n";
    Printf.printf "║                                                        ║\n";
    Printf.printf "╚════════════════════════════════════════════════════════╝\n";
    Printf.printf "\n";
    let choice = ask_int "Votre choix (1-6) : " in
    match choice with
    | 1 -> mode_play_against_strategy ()
    | 2 -> mode_strategy_vs_strategy ()
    | 3 -> mode_sfp_rounds ()
    | 4 -> mode_sfp_time ()
    | 5 -> mode_sfp_rounds_uniform ()
    | 6 -> mode_sfp_time_uniform ()
    | _ -> Printf.printf "Choix invalide. Veuillez relancer le programme.\n"
