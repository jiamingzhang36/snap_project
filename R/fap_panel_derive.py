"""
Derive log and share variables from FAP merged panel; check and fix identity consistency.

Input:  FAP_ALL_YEARS_COMBINED_MERGED.numbers (preferred) or .csv
        (county, year, month, cases, recipients, ...)
Output: fap_panel_derived.csv with ln(cases), ln(recipients), ln(payments),
        adult_share, child_share, and identity-consistent avg columns.

Identities (enforced by recomputing from levels when inconsistent):
  avg_recipients_per_case = recipients / cases
  avg_per_person         = payments / recipients
  avg_per_case           = payments / cases
"""

import pandas as pd
import numpy as np
from pathlib import Path

# Paths
BASE = Path(__file__).resolve().parent.parent
DATA_CLEAN = BASE / "data_clean"
FAP_DIR = BASE / "FAP"
IN_NUMBERS = DATA_CLEAN / "FAP_ALL_YEARS_COMBINED_MERGED.numbers"
if not IN_NUMBERS.exists():
    IN_NUMBERS = FAP_DIR / "FAP_ALL_YEARS_COMBINED_MERGED.numbers"
IN_CSV = DATA_CLEAN / "FAP_ALL_YEARS_COMBINED_MERGED.csv"
if not IN_CSV.exists():
    IN_CSV = FAP_DIR / "FAP_ALL_YEARS_COMBINED_MERGED.csv"
OUT_CSV = DATA_CLEAN / "fap_panel_derived.csv"
DIAG_CSV = DATA_CLEAN / "fap_identity_diagnostics.csv"


def _load_numbers(path: Path) -> pd.DataFrame:
    """Load first sheet of a .numbers file into a DataFrame. Requires: pip install numbers-parser"""
    try:
        from numbers_parser import Document
    except ImportError:
        raise ImportError(
            "Reading .numbers requires: pip install numbers-parser (macOS: brew install snappy if needed)"
        )
    doc = Document(str(path))
    table = doc.sheets[0].tables[0]
    rows = table.rows(values_only=True)
    if not rows:
        raise ValueError(f"Empty table in {path}")
    headers = [(h or "").strip() for h in rows[0]]
    df = pd.DataFrame(rows[1:], columns=headers)
    # drop completely empty rows
    df = df.dropna(how="all")
    return df


def _load_raw() -> pd.DataFrame:
    """Load raw panel: prefer .numbers, fallback to .csv."""
    if IN_NUMBERS.exists():
        print(f"Using source: {IN_NUMBERS}")
        df = _load_numbers(IN_NUMBERS)
        # Normalize column names to match CSV (strip, lowercase) so "County"/"Year" etc. work
        df.columns = [str(c).strip().lower() for c in df.columns]
        return df
    if IN_CSV.exists():
        print(f"Using source: {IN_CSV}")
        return pd.read_csv(IN_CSV, dtype={"county": str})
    raise FileNotFoundError(
        f"Input not found. Put either FAP_ALL_YEARS_COMBINED_MERGED.numbers or .csv in:\n  {DATA_CLEAN}\n  or {FAP_DIR}"
    )


def main():
    df = _load_raw()
    df["county"] = df["county"].astype(str)
    df["year"] = pd.to_numeric(df["year"], errors="coerce")
    df["month"] = pd.to_numeric(df["month"], errors="coerce")

    # Column names (as in CSV)
    cases_col = "cases"
    recip_col = "recipients"
    adult_col = "adult recipients"
    child_col = "child recipients"
    pay_col = "payments"
    avg_case_col = "average per case"
    avg_person_col = "average per person"
    avg_rec_case_col = "avg. recipients per case"

    for c in [cases_col, recip_col, adult_col, child_col, pay_col]:
        if c not in df.columns:
            raise KeyError(f"Missing column: {c}")
        df[c] = pd.to_numeric(df[c], errors="coerce")

    # ----- 1) Logs (avoid log(0)) -----
    df["ln_cases"] = np.log(df[cases_col].clip(lower=1e-6))
    df["ln_recipients"] = np.log(df[recip_col].clip(lower=1e-6))
    df["ln_payments"] = np.log(df[pay_col].clip(lower=1e-6))

    # ----- 2) Shares -----
    df["adult_share"] = np.where(
        df[recip_col] > 0,
        df[adult_col] / df[recip_col],
        np.nan
    )
    df["child_share"] = np.where(
        df[recip_col] > 0,
        df[child_col] / df[recip_col],
        np.nan
    )

    # ----- 3) Identities: compute from levels -----
    df["_rec_per_case"] = np.where(df[cases_col] > 0, df[recip_col] / df[cases_col], np.nan)
    df["_pay_per_person"] = np.where(df[recip_col] > 0, df[pay_col] / df[recip_col], np.nan)
    df["_pay_per_case"] = np.where(df[cases_col] > 0, df[pay_col] / df[cases_col], np.nan)

    # Tolerate small relative/absolute difference (rounding, reporting)
    rtol, atol = 1e-3, 0.5

    # Reported columns (may be missing in CSV)
    if avg_rec_case_col not in df.columns:
        df[avg_rec_case_col] = np.nan
    if avg_person_col not in df.columns:
        df[avg_person_col] = np.nan
    if avg_case_col not in df.columns:
        df[avg_case_col] = np.nan

    reported_rec = pd.to_numeric(df[avg_rec_case_col], errors="coerce")
    reported_person = pd.to_numeric(df[avg_person_col], errors="coerce")
    reported_case = pd.to_numeric(df[avg_case_col], errors="coerce")

    # Check consistency
    ok_rec = np.isclose(df["_rec_per_case"], reported_rec, rtol=rtol, atol=atol, equal_nan=True)
    ok_person = np.isclose(df["_pay_per_person"], reported_person, rtol=rtol, atol=atol, equal_nan=True)
    ok_case = np.isclose(df["_pay_per_case"], reported_case, rtol=rtol, atol=atol, equal_nan=True)

    # Diagnostics: rows where any identity fails
    diag = df[["county", "year", "month"]].copy()
    diag["ok_avg_recip_per_case"] = ok_rec
    diag["ok_avg_per_person"] = ok_person
    diag["ok_avg_per_case"] = ok_case
    diag["computed_avg_recip_per_case"] = df["_rec_per_case"]
    diag["computed_avg_per_person"] = df["_pay_per_person"]
    diag["computed_avg_per_case"] = df["_pay_per_case"]
    diag["reported_avg_recip_per_case"] = reported_rec
    diag["reported_avg_per_person"] = reported_person
    diag["reported_avg_per_case"] = reported_case
    diag.to_csv(DIAG_CSV, index=False)
    n_incons = (~(ok_rec & ok_person & ok_case)).sum()
    print(f"Identity check: {n_incons} rows inconsistent (see {DIAG_CSV})")

    # ----- 4) Fix: overwrite reported with computed so panel is consistent -----
    df[avg_rec_case_col] = df["_rec_per_case"]
    df[avg_person_col] = df["_pay_per_person"]
    df[avg_case_col] = df["_pay_per_case"]
    df = df.drop(columns=["_rec_per_case", "_pay_per_person", "_pay_per_case"])

    # Reorder: id cols, then levels, then derived
    id_cols = ["county", "year", "month"]
    level_cols = [c for c in [cases_col, recip_col, adult_col, child_col, pay_col] if c in df.columns]
    avg_cols = [c for c in [avg_case_col, avg_person_col, avg_rec_case_col] if c in df.columns]
    derived = ["ln_cases", "ln_recipients", "ln_payments", "adult_share", "child_share"]
    rest = [c for c in df.columns if c not in id_cols + level_cols + avg_cols + derived]
    df = df[id_cols + level_cols + avg_cols + derived + rest]

    df.to_csv(OUT_CSV, index=False, encoding="utf-8-sig")
    print(f"Wrote {OUT_CSV}  rows={len(df)}")
    print("Columns:", list(df.columns))


if __name__ == "__main__":
    main()
