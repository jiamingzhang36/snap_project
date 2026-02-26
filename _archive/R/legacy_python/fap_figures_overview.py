"""
FAP report overview figures: state-level Version A (2A, 2B, 3A, 3B) + one county-level figure.

State level: aggregate all counties to Michigan state totals per year-month.
County level: quantile bands of ln(cases) over time.

State-level: only year-months with full county coverage (83) are summed; 2022-10–2023-09
are excluded (GreenBook has 59 counties only), so the state series has a gap there.

Outputs (in outputs/fap_overview/):
  fig_2A.png, fig_2B.png, fig_3A.png, fig_3B.png  (single-panel, for report)
  fig_2A_2B.png, fig_3A_3B.png                    (two-panel overview)
  fig_county_quantiles.png                        (county ln(cases) 10th–90th percentiles)
  fig_4.png                                      (state avg per person $)
  fig_5.png                                      (payments: actual vs scale vs intensity counterfactuals)
  fig_6.png                                      (county avg per case P10/P50/P90 bands)

Run: python3 R/fap_figures_overview.py
Or from notebook: %run R/fap_figures_overview.py (from project root)
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path

BASE = Path(__file__).resolve().parent.parent
DATA_CLEAN = BASE / "data_clean"
OUT_DIR = BASE / "outputs" / "fap_overview"
DERIVED_CSV = DATA_CLEAN / "fap_panel_derived.csv"

# Fallback if derived not present
if not DERIVED_CSV.exists():
    DERIVED_CSV = DATA_CLEAN / "FAP_ALL_YEARS_COMBINED_MERGED.csv"

OUT_DIR.mkdir(parents=True, exist_ok=True)


def _date_axis(df: pd.DataFrame) -> pd.Series:
    """Year-month to datetime for plotting."""
    return pd.to_datetime(
        df["year"].astype(str) + "-" + df["month"].astype(str) + "-01",
        format="%Y-%m-%d",
        errors="coerce",
    )


# Full state = 83 Michigan counties. GreenBook (2022-10 to 2023-09) has only 59 counties.
# State totals are only computed for full-coverage months to avoid level jumps.
FULL_STATE_N_COUNTIES = 83


def load_and_aggregate_state() -> pd.DataFrame:
    """Load county panel and aggregate to state level (Michigan). Only use year-months with full county coverage (83)."""
    df = pd.read_csv(DERIVED_CSV, dtype={"county": str})
    df["year"] = pd.to_numeric(df["year"], errors="coerce")
    df["month"] = pd.to_numeric(df["month"], errors="coerce")
    df = df.dropna(subset=["year", "month"])

    n_counties = df.groupby(["year", "month"])["county"].transform("nunique")
    df_full = df[n_counties == FULL_STATE_N_COUNTIES].copy()

    agg = (
        df_full.groupby(["year", "month"], as_index=False)
        .agg(
            cases=("cases", "sum"),
            recipients=("recipients", "sum"),
            adult_recipients=("adult recipients", "sum"),
            child_recipients=("child recipients", "sum"),
            payments=("payments", "sum"),
        )
    )
    agg["date_cal"] = _date_axis(agg)
    agg["ln_cases"] = np.log(agg["cases"].clip(lower=1))
    agg["ln_recipients"] = np.log(agg["recipients"].clip(lower=1))
    agg["ln_payments"] = np.log(agg["payments"].clip(lower=1))
    agg["adult_share"] = np.where(
        agg["recipients"] > 0,
        agg["adult_recipients"] / agg["recipients"],
        np.nan,
    )
    agg["avg_per_case"] = np.where(
        agg["cases"] > 0,
        agg["payments"] / agg["cases"],
        np.nan,
    )
    agg["avg_per_person"] = np.where(
        agg["recipients"] > 0,
        agg["payments"] / agg["recipients"],
        np.nan,
    )
    return agg.sort_values("date_cal").reset_index(drop=True)


def plot_state_panel(
    ax,
    state: pd.DataFrame,
    y_col: str,
    ylabel: str,
    title: str,
    color: str = "C0",
):
    """Single state-level time series panel."""
    ax.plot(state["date_cal"], state[y_col], color=color, linewidth=1.5)
    ax.set_ylabel(ylabel)
    ax.set_title(title)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.grid(True, alpha=0.3)
    ax.tick_params(axis="x", rotation=0)


def fig_2A_2B(state: pd.DataFrame):
    """Fig 2A: State cases; Fig 2B: State recipients (levels). Single figure + separate files."""
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(10, 4))
    plot_state_panel(
        ax1, state, "cases", "Cases", "2A. State total cases", color="C0"
    )
    plot_state_panel(
        ax2, state, "recipients", "Recipients", "2B. State total recipients", color="C1"
    )
    fig.tight_layout()
    fig.savefig(OUT_DIR / "fig_2A_2B.png", dpi=150, bbox_inches="tight")
    plt.close()
    print("Saved", OUT_DIR / "fig_2A_2B.png")
    # Individual 2A, 2B for report
    for label, col, ylab, fname in [
        ("2A. State total cases", "cases", "Cases", "fig_2A.png"),
        ("2B. State total recipients", "recipients", "Recipients", "fig_2B.png"),
    ]:
        fig, ax = plt.subplots(figsize=(5, 3.5))
        plot_state_panel(ax, state, col, ylab, label)
        fig.tight_layout()
        fig.savefig(OUT_DIR / fname, dpi=150, bbox_inches="tight")
        plt.close()
        print("Saved", OUT_DIR / fname)


def fig_3A_3B(state: pd.DataFrame):
    """Fig 3A: State payments (million $); Fig 3B: State avg per case ($). Single figure + separate files."""
    state = state.copy()
    state["payments_mill"] = state["payments"] / 1e6  # so y-axis shows 200, 250, not 2e8
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(10, 4))
    plot_state_panel(
        ax1, state, "payments_mill", "Payments (million $)", "3A. State total payments", color="C2"
    )
    plot_state_panel(
        ax2,
        state,
        "avg_per_case",
        "Avg per case ($)",
        "3B. State avg payment per case",
        color="C3",
    )
    fig.tight_layout()
    fig.savefig(OUT_DIR / "fig_3A_3B.png", dpi=150, bbox_inches="tight")
    plt.close()
    print("Saved", OUT_DIR / "fig_3A_3B.png")
    # Individual 3A, 3B for report
    fig, ax = plt.subplots(figsize=(5, 3.5))
    plot_state_panel(ax, state, "payments_mill", "Payments (million $)", "3A. State total payments")
    fig.tight_layout()
    fig.savefig(OUT_DIR / "fig_3A.png", dpi=150, bbox_inches="tight")
    plt.close()
    print("Saved", OUT_DIR / "fig_3A.png")
    fig, ax = plt.subplots(figsize=(5, 3.5))
    plot_state_panel(ax, state, "avg_per_case", "Avg per case ($)", "3B. State avg payment per case")
    fig.tight_layout()
    fig.savefig(OUT_DIR / "fig_3B.png", dpi=150, bbox_inches="tight")
    plt.close()
    print("Saved", OUT_DIR / "fig_3B.png")


def fig_county_quantiles(df: pd.DataFrame):
    """County-level: quantile bands of ln(cases) over time."""
    df = df.copy()
    df["date_cal"] = _date_axis(df)
    df = df.dropna(subset=["date_cal"])
    if "ln_cases" not in df.columns:
        df["ln_cases"] = np.log(df["cases"].clip(lower=1))

    by_date = df.groupby("date_cal")["ln_cases"].quantile([0.1, 0.25, 0.5, 0.75, 0.9]).unstack()
    by_date = by_date.sort_index()
    x = by_date.index

    fig, ax = plt.subplots(figsize=(8, 4))
    ax.fill_between(x, by_date[0.1], by_date[0.9], alpha=0.2, color="C0")
    ax.fill_between(x, by_date[0.25], by_date[0.75], alpha=0.3, color="C0")
    ax.plot(x, by_date[0.5], color="C0", linewidth=2, label="Median ln(cases)")
    ax.set_ylabel("ln(cases)")
    ax.set_xlabel("Date")
    ax.set_title("County-level ln(cases): 10th–90th and 25th–75th percentiles")
    ax.legend(loc="best")
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.grid(True, alpha=0.3)
    fig.tight_layout()
    fig.savefig(OUT_DIR / "fig_county_quantiles.png", dpi=150, bbox_inches="tight")
    plt.close()
    print("Saved", OUT_DIR / "fig_county_quantiles.png")


def fig_4_avg_per_person(state: pd.DataFrame):
    """Fig 4: State avg per person (payments/recipients). Divergence from avg per case suggests household size/composition."""
    fig, ax = plt.subplots(figsize=(6, 3.5))
    plot_state_panel(
        ax, state, "avg_per_person", "Avg per person ($)",
        "4. State avg payment per recipient (avg per person)",
        color="C4",
    )
    fig.tight_layout()
    fig.savefig(OUT_DIR / "fig_4.png", dpi=150, bbox_inches="tight")
    plt.close()
    print("Saved", OUT_DIR / "fig_4.png")


def fig_5_payments_decomposition(state: pd.DataFrame):
    """Fig 5: Actual payments vs two counterfactuals (2019 anchor).
    CF scale: actual cases × 2019 avg per case (if intensity unchanged).
    CF intensity: 2019 cases × actual avg per case (if scale unchanged)."""
    state = state.copy()
    ref = state[state["year"] == 2019]
    if ref.empty:
        print("[WARN] No 2019 data; skip fig_5")
        return
    cases_2019 = ref["cases"].mean()
    avg_per_case_2019 = ref["avg_per_case"].mean()

    state["payments_actual_mill"] = state["payments"] / 1e6
    state["cf_scale_mill"] = (state["cases"] * avg_per_case_2019) / 1e6
    state["cf_intensity_mill"] = (cases_2019 * state["avg_per_case"]) / 1e6

    fig, ax = plt.subplots(figsize=(8, 4))
    ax.plot(state["date_cal"], state["payments_actual_mill"], color="C0", linewidth=2, label="Actual payments")
    ax.plot(state["date_cal"], state["cf_scale_mill"], color="C1", linewidth=1.2, linestyle="--", label="If intensity = 2019 (scale only)")
    ax.plot(state["date_cal"], state["cf_intensity_mill"], color="C2", linewidth=1.2, linestyle="--", label="If scale = 2019 (intensity only)")
    ax.set_ylabel("Payments (million $)")
    ax.set_xlabel("Date")
    ax.set_title("5. Payments: actual vs scale vs intensity (2019 anchor)")
    ax.legend(loc="best", fontsize=8)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.grid(True, alpha=0.3)
    fig.tight_layout()
    fig.savefig(OUT_DIR / "fig_5.png", dpi=150, bbox_inches="tight")
    plt.close()
    print("Saved", OUT_DIR / "fig_5.png")


def fig_6_county_avg_per_case_quantiles(df: pd.DataFrame):
    """Fig 6: County-level avg per case (payments/cases) P10/P50/P90 over time. Parallel shift = common shock; widening = heterogeneous."""
    df = df.copy()
    df["date_cal"] = _date_axis(df)
    df = df.dropna(subset=["date_cal"])
    if "average per case" in df.columns:
        df["avg_per_case"] = pd.to_numeric(df["average per case"], errors="coerce")
    else:
        df["cases"] = pd.to_numeric(df["cases"], errors="coerce")
        df["payments"] = pd.to_numeric(df["payments"], errors="coerce")
        df["avg_per_case"] = np.where(df["cases"] > 0, df["payments"] / df["cases"], np.nan)
    df = df.dropna(subset=["avg_per_case"])

    by_date = df.groupby("date_cal")["avg_per_case"].quantile([0.1, 0.5, 0.9]).unstack()
    by_date = by_date.sort_index()
    x = by_date.index

    fig, ax = plt.subplots(figsize=(8, 4))
    ax.fill_between(x, by_date[0.1], by_date[0.9], alpha=0.25, color="C0")
    ax.plot(x, by_date[0.5], color="C0", linewidth=2, label="Median avg per case ($)")
    ax.plot(x, by_date[0.1], color="C0", linewidth=0.8, alpha=0.7, linestyle=":")
    ax.plot(x, by_date[0.9], color="C0", linewidth=0.8, alpha=0.7, linestyle=":")
    ax.set_ylabel("Avg per case ($)")
    ax.set_xlabel("Date")
    ax.set_title("6. County avg per case: P10 / P50 / P90 (intensity distribution)")
    ax.legend(loc="best")
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.grid(True, alpha=0.3)
    fig.tight_layout()
    fig.savefig(OUT_DIR / "fig_6.png", dpi=150, bbox_inches="tight")
    plt.close()
    print("Saved", OUT_DIR / "fig_6.png")


def main():
    # State-level
    state = load_and_aggregate_state()
    fig_2A_2B(state)
    fig_3A_3B(state)
    fig_4_avg_per_person(state)
    fig_5_payments_decomposition(state)

    # County-level
    df = pd.read_csv(DERIVED_CSV, dtype={"county": str})
    df["year"] = pd.to_numeric(df["year"], errors="coerce")
    df["month"] = pd.to_numeric(df["month"], errors="coerce")
    fig_county_quantiles(df)
    fig_6_county_avg_per_case_quantiles(df)

    print("Done. Figures in", OUT_DIR)


if __name__ == "__main__":
    main()
