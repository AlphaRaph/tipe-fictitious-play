# TIPE 2024-2025: Solving Geister with Smooth Fictitious Play and Imperfect Information Minimax

This repository contains the algorithms developed for my TIPE (*Travail d'Initiative Personnelle Encadré*) during my two years of CPGE MP2I-MPI* (*Classes Préparatoires aux Grandes Ecoles Mathématiques Physique et Informatique*). The goal of this project is to apply game theory approaches to computationally solve or find strong optimal strategies for the board game **[Geister](https://en.wikipedia.org/wiki/Good_%26_Bad_Ghosts)** (Good and Bad Ghosts), a zero-sum game with imperfect information.

## Overview

[Geister](https://en.wikipedia.org/wiki/Good_%26_Bad_Ghosts) is a game where players control "good" (blue) and "bad" (red) ghosts. The defining mechanic is that a player **cannot see the colors of their opponent's ghosts**. This imperfect information introduces complex bluffing and makes standard perfect-information game tree searches (like simple Minimax) insufficient to find a mathematically bulletproof Nash Equilibrium.

We explore several algorithmic approaches to tackle this game, progressively increasing in mathematical correctness and computational complexity, ultimately culminating in an advanced **Smooth Fictitious Play (SFP)** algorithm.

## Code Architecture / Components

### 🥇 The Flagship Algorithm: `geister_smooth_fictitious_play.ml`
This is the core achievement of the project. It implements **Smooth Fictitious Play** heavily tailored for imperfect information trees. 
- It maintains the accumulated regrets over thousands of iterations to compute optimal empirical mixed strategies, pushing the game towards a **Nash Equilibrium**.
- **Resumable pipelines:** Capable of pausing and seamlessly resuming massive calculations from serialized `Hashtbl` `.dat` states, with intelligent lambda progression math tracking.
- **Dynamic Wrapper Generation:** Generates output `.ml` strategy files dynamically, injecting base heuristic source code directly into the compiler. This ensures the output strategies are fully self-contained and portable without OCaml versioning or module dependency crashes.

**Key Metrics Tracked:**
To measure the algorithm's convergence and evaluate the resulting strategies, we track three fundamental metrics:

- **Expected Payoff / Score ($u_i$):** Corresponds to the mathematically expected game outcome ranging from -1.0 to +1.0. A score of +1.0 is a guaranteed win, -1.0 is a guaranteed loss, and 0.0 represents a perfectly balanced expected outcome. This serves as the base unit of utility in our zero-sum game.

- **Regret ($R_i$):** Represents the difference in payoff between the best possible strategy a player could have chosen against the opponents' current strategies, and the utility of the strategy they actually played. Formally, for a strategy profile $x$, Player $i$'s regret is:
  $$ R_i(x) = \sup_{y_i \in X_i} u_i(y_i, x_{-i}) - u_i(x) $$
  where $u_i$ is the utility function of Player $i$ (based on scoring), $X_i$ is their strategy set, and $x_{-i}$ denotes the strategies of the other players.

- **Exploitability ($\Phi$):** Measures how far a strategy profile is from a Nash equilibrium. Also known as NashConv, it is defined formally as the sum of the regrets of all players:
  $$ \Phi(x) = \sum_{i \in I} R_i(x) $$
  It is non-negative everywhere and zero precisely at Nash equilibrium.

> *Source: The mathematical formalizations for Regret and Exploitability are directly taken from [ApproxED: Approximate exploitability descent via learned best responses](https://ifaamas.csc.liv.ac.uk/Proceedings/aamas2025/pdfs/p1454.pdf).*

### Smooth Fictitious Play Results

We successfully trained the SFP algorithm over a 10-hour period, identifying an **$\epsilon$-Nash equilibrium where $\epsilon = 0.27$** (corresponding to the supremum of the regrets). To achieve this convergence, we applied an exponential variation to the smoothing parameter $\lambda$ with an initial value of 0.01 and an $\alpha$ multiplier of 1.05. The initial strategies were our heuristic strategy (`strategies/heuristic.ml`).

The resulting strategies computed from this training session are exported as standalone code in:
- `strategies/p1t1679d10exp0.0-1.050.ml`
- `strategies/p2t1679d10exp0.0-1.050.ml`

**Final Training Metrics:**
- **P1 Regret:** 0.27
- **P2 Regret:** 0.16
- **Exploitability ($\Phi$):** 0.43

**Empirical Performance:**
- **Player 1 Strategy:** Scored **+0.4590** against a uniform player (strategies/uniform.ml), and **-0.1653** against our heuristic player (strategies/heuristic.ml).
- **Player 2 Strategy:** Scored **+0.3337** against a uniform player (strategies/uniform.ml), and **-0.2369** against our heuristic player (strategies/heuristic.ml).

**Conclusion:** 
While the empirically computed SFP strategies exhibit significantly lower overall exploitability than the hand-crafted heuristics—meaning they are mathematically less predictable and more robust against optimal adversaries in general—they currently still lose in direct, targeted matchups against the specialized heuristic engine. To discover a tighter $\epsilon$-Nash equilibrium ($\epsilon \to 0$) and ultimately beat the heuristic bot consistently, further hyperparameter tuning of the $\lambda$ schedule and extended training sessions are required.

![SFP 10-hour exponential lambda variation (Initial $\lambda = 0.01$, $\alpha = 1.05$)](reports/sfp_time36000.0d10exp0.0-1.050.png)


### 🥈 Intermediate Explorations
- **`geister_classic_minimax.ml`**: A foundational attempt using standard **Minimax with Alpha-Beta pruning** and **Iterative deepening depth-first search**. To handle imperfect information, it averages the outcomes or takes the worst outcome of all possible hidden states. It acts as a good standalone bot to play against, but cannot bluff so it is an easy win for a human user. That's why we need to focus on imperfect-information algorithms. 
- **`geister_best_response_dynamics.ml`**: An earlier iteration demonstrating **Iterated Best Response Dynamics**. It uses **Imperfect Information Minimax** and **Alpha-Beta Pruning** to compute the best response against the opponent's current strategy. These algorithms are sourced from two 1993 papers from researchers at the University of Tennessee ([IMP Minimax paper](https://cdn.aaai.org/Symposia/Fall/1993/FS-93-02/FS93-02-009.pdf), [IMP Alpha-beta paper](https://cdn.aaai.org/Symposia/Fall/1993/FS-93-02/FS93-02-010.pdf)). This script implements Best Response Dynamics as suggested in the *5. Extensions and open questions* of the first paper, though there are no strict theoretical convergence guarantees for this method. It perfectly illustrates how pure response dynamics fail to converge in imperfect information games, falling into endless loops. The reason is that Geister doesn't have a pure Nash equilibrium, as knowing the placement of the opponent ghost means winning the game. Thus we need to focus on fictitious play methods to compute mixed strategies. 
- **`geister_fictitious_play.ml`**: A strict implementation of mathematical [Fictitious Play](https://en.wikipedia.org/wiki/Fictitious_play). Unlike the previous iterative best response approach, this algorithm tracks the accumulation of all pure strategies played over time. The final output strategy is simply **the average mixture of all historical best responses**, forcing the strategy distribution to slowly converge towards a more stable equilibrium. However the convergence is slow, which is why we eventually implemented a Smooth Fictitious Play method to increase convergence speed as shown in a [2020 lecture of Justin Kang from the University of Toronto](https://www.youtube.com/watch?v=-wiA5yC5Iek).

### 🎮 The CLI Interface: `main.ml`
The main entry point of the project. It provides an intuitive, interactive text-based terminal menu with dynamic routing allowing you to:
- Play against the resulting strategies as a human.
- Pit two AI strategies against one another in an automated match (useful to evaluate empirical performance).
- Launch a new **Smooth Fictitious Play** training session (with stop parameters by *time* or by *round limit*).
- Resume a previously stopped **SFP** training pipeline.

## How to Compile and Run

Make sure you have `ocaml`, `ocamlfind`, and the `dynlink` package installed.

To compile and launch the main interactive program:
```bash
ocamlfind ocamlopt -linkpkg -package dynlink geister_smooth_fictitious_play.ml main.ml -o geister
./geister
```
### Testing Intermediate Scripts

The intermediate scripts can be tested directly outside of the main CLI.

**1. Classic Minimax (Standalone CLI)**
If you just want to play the classical average-based Minimax against the machine directly, you can compile it separately into a standalone executable:
```bash
ocamlopt -o geister_minimax geister_classic_minimax.ml
./geister_minimax
```

**2. Fictitious Play Implementations (REPL)**
The `geister_best_response_dynamics.ml` and `geister_fictitious_play.ml` scripts do not have a dedicated Command Line Interface. They are intermediate algorithmic milestones that guided our research toward the final SFP methodology. 

To manually test their specific functions, use the OCaml REPL (`utop`):
1. Launch the interactive toplevel by running `utop` in your terminal.
2. Load the desired script using the `#use` directive:
   ```ocaml
   #use "geister_fictitious_play.ml";;
   ```
3. You can now interactively call the specific functions defined at the end of the file.


## References & Resources

During the development of this TIPE, the following resources were instrumental in understanding the underlying game theory and implementing the algorithms:

- **Carlos Martin & Tuomas Sandholm (Carnegie Mellon University, 2025):** [ApproxED: Approximate exploitability descent via learned best responses](https://ifaamas.csc.liv.ac.uk/Proceedings/aamas2025/pdfs/p1454.pdf). This publication provided the formal mathematical definitions for assessing strategic convergence via Exploitability and Regret metrics in imperfect information games.

- **Justin Kang (University of Toronto, 2020):** Video lecture on [Fictitious Play and Smooth Fictitious Play](https://www.youtube.com/watch?v=-wiA5yC5Iek). This course served as the primary theoretical foundation for understanding the mechanics of the SFP algorithm.

- **Jean R. S. Blair, David Mutchler, & Cheng Liu (University of Tennessee, 1993):** [A Minimax Algorithm for Imperfect Information Games](https://cdn.aaai.org/Symposia/Fall/1993/FS-93-02/FS93-02-009.pdf). Defines a Minimax algorithm for games with imperfect information and perfect recall.

- **Michael van Lent & David Mutchler (University of Tennessee, 1993):** [A Pruning Algorithm for Imperfect Information Games](https://cdn.aaai.org/Symposia/Fall/1993/FS-93-02/FS93-02-010.pdf). Extends the prior work into a more efficient Alpha-Beta pruning algorithm for imperfect information trees.


## Algorithmic Challenges

A major computational difficulty encountered during this project stems from the theoretical constraints of the algorithms. The **Imperfect Information Minimax** (IMP Minimax) algorithm guarantees correctness only in games with **perfect recall**. Consequently, the algorithm must maintain the entire history of the game state. This strict requirement prevents the use of standard state memoization, causing the time and space complexity to grow exponentially (rather than linearly) relative to the size of the game tree.

## Related Work

Other researchers have also explored computational approaches to solve Geister, utilizing different algorithmic paradigms:

- **University of Tokyo (IEEE CoG 2022):** [Tackling Geister with Depth-Limited CFR and Belief States](https://ieee-cog.org/2022/assets/papers/paper_136.pdf). Rather than Fictitious Play, this paper achieves an $\epsilon$-Nash equilibrium using a variant of **Counterfactual Regret Minimization (CFR)** combined with depth-limited search and belief states.

## Context
Author: Raphaël BELLIOT
Year: 2024-2025
Context: CPGE MP2I-MPI* TIPE (Computer Science)
