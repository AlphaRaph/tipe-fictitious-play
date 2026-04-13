(* ==========================================================================
   Stratégie : Uniforme
   Description : Joue un coup valide avec une probabilité strictement égale.
   Joue le rôle de baseline ou d'adversaire imprévisible mais très faible.
   ========================================================================== *)

open Geister_smooth_fictitious_play

let alt_probs (g : game) (i : info_set) : (alternative * float) list =
  let alts = GameTree.alternatives [(i, 1.)] in 
  let n = List.length alts in 
  List.map (fun alt -> (alt, 1. /. (float n))) alts

(** Enregistrement pour le chargement dynamique **)
let () = StrategyRegistry.register alt_probs
