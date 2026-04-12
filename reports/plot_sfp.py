#!/usr/bin/env python3
"""
plot_sfp.py — Visualisation de la convergence du Smooth Fictitious Play

Usage : python plot_sfp.py <fichier.csv>
Exemple : python plot_sfp.py sfp_t60d10l100.csv

Le fichier CSV doit avoir les colonnes :
    tour_p1, tour_p2, regret_p1, regret_p2, exploitabilite, temps_ecoule
"""

import sys
import csv
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
from pathlib import Path


def load_csv(path: str):
    rows = []
    with open(path, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        for row in reader:
            rows.append({k: float(v) for k, v in row.items()})
    return rows


def plot(csv_path: str):
    rows = load_csv(csv_path)[1:]

    # Axe x : tours complets (2 demi-tours par unité)
    x = [(i + 1) / 2.0 for i in range(len(rows))]
    r1     = [r["regret_p1"]      for r in rows]
    r2     = [r["regret_p2"]      for r in rows]
    expl   = [r["exploitabilite"] for r in rows]
    temps  = [r["temps_ecoule"]   for r in rows]

    # ---- Figure ----
    plt.style.use("dark_background")
    fig, ax = plt.subplots(figsize=(13, 7))

    # Exploitabilité — mise en valeur
    ax.plot(x, expl, color="#00f2ff", linewidth=2.5,
            label="Exploitabilité (Nash Gap)", zorder=5)
    ax.fill_between(x, expl, color="#00f2ff", alpha=0.08)

    # Regrets individuels
    ax.plot(x, r1, color="#ff4b6e", linewidth=1.2,
            linestyle="--", alpha=0.75, label="Regret P1")
    ax.plot(x, r2, color="#a855f7", linewidth=1.2,
            linestyle="--", alpha=0.75, label="Regret P2")

    # Ligne zéro
    ax.axhline(0, color="white", linewidth=0.5, alpha=0.3)

    # Marquer les tours complets (lignes verticales)
    for xi in range(1, int(max(x)) + 1):
        ax.axvline(xi, color="white", linewidth=0.3, alpha=0.15, linestyle=":")

    # Annotations
    stem = Path(csv_path).stem
    ax.set_title(f"Convergence SFP — {stem}",
                 fontsize=15, fontweight="bold", pad=16, color="white")
    ax.set_xlabel("Tours complets (Itérations)", fontsize=11, color="#cccccc")
    ax.set_ylabel("Valeur (regret / exploitabilité)", fontsize=11, color="#cccccc")
    
    # Correction : Forcer l'affichage uniquement des entiers sur l'axe X
    ax.xaxis.set_major_locator(ticker.MaxNLocator(integer=True))

    # Axe secondaire : temps écoulé
    ax2 = ax.twiny()
    ax2.set_xlim(ax.get_xlim())
    tick_positions = list(range(0, len(x) + 1, max(1, len(x) // 8)))
    tick_labels = [f"{temps[i-1]:.0f}s" if i > 0 else "0s" for i in tick_positions]
    ax2.set_xticks(tick_positions)
    ax2.set_xticklabels(tick_labels, fontsize=8, color="#cccccc")
    ax2.set_xlabel("Temps écoulé", fontsize=9, color="#cccccc")

    ax.legend(fontsize=11, loc="upper right", frameon=True, framealpha=0.3)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

    plt.tight_layout()

    # Sauvegarde + affichage
    out = Path(csv_path).with_suffix(".png")
    plt.savefig(out, dpi=200, bbox_inches="tight")
    print(f"[OK] Graphique enregistré : {out}")
    plt.show()


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)
    plot(sys.argv[1])
