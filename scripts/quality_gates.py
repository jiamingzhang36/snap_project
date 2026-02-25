#!/usr/bin/env python3
import argparse
import csv
import json
from dataclasses import dataclass, asdict
from datetime import datetime
from pathlib import Path


@dataclass
class Gate:
    name: str
    status: str  # PASS / FAIL / WARN
    detail: str


def csv_rows(path: Path):
    if not path.exists():
        return None
    with path.open("r", encoding="utf-8", newline="") as f:
        rows = list(csv.reader(f))
    return max(len(rows) - 1, 0) if rows else 0


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--root", default=".", help="project root")
    args = ap.parse_args()

    root = Path(args.root).resolve()
    out_logs = root / "outputs" / "logs"
    out_logs.mkdir(parents=True, exist_ok=True)

    gates = []

    required = [
        root / "README.md",
        root / "PROJECT_RULES.md",
        root / "R" / "99_run" / "run_all.R",
        root / "R" / "06_food_behavior" / "run_food_behavior.R",
        root / "dewey-downloads" / "snap" / "michigan_county_weekly_behavior_panel.parquet",
        root / "outputs" / "food_behavior" / "tables" / "food_behavior_event_study_abawd.csv",
        root / "outputs" / "food_behavior" / "tables" / "food_behavior_robustness_abawd.csv",
        root / "outputs" / "food_behavior" / "tables" / "food_behavior_unemp_dl_cumulative.csv",
    ]
    miss = [str(p) for p in required if not p.exists()]
    gates.append(Gate("required_files", "PASS" if not miss else "FAIL", "ok" if not miss else f"missing={miss}"))

    stray = []
    for d in [root / "outputs" / "tables", root / "outputs" / "figures"]:
        if d.exists():
            stray.extend([str(p) for p in d.glob("food_behavior_*")])
    gates.append(Gate("no_stray_food_behavior", "PASS" if not stray else "FAIL", "ok" if not stray else f"stray={stray}"))

    key_csv = [
        root / "outputs" / "food_behavior" / "tables" / "food_behavior_event_study_abawd.csv",
        root / "outputs" / "food_behavior" / "tables" / "food_behavior_robustness_abawd.csv",
        root / "outputs" / "food_behavior" / "tables" / "food_behavior_unemp_dl_main.csv",
    ]
    bad = []
    for p in key_csv:
        n = csv_rows(p)
        if n is None or n <= 0:
            bad.append((str(p), n))
    gates.append(Gate("key_csv_nonempty", "PASS" if not bad else "FAIL", "ok" if not bad else f"bad={bad}"))

    junk = list(root.rglob(".DS_Store")) + list(root.rglob(".Rapp.history"))
    gates.append(Gate("junk_files", "PASS" if not junk else "WARN", "ok" if not junk else f"count={len(junk)}"))

    summary = {
        "pass": sum(1 for g in gates if g.status == "PASS"),
        "warn": sum(1 for g in gates if g.status == "WARN"),
        "fail": sum(1 for g in gates if g.status == "FAIL"),
    }

    payload = {
        "generated_at": datetime.now().isoformat(timespec="seconds"),
        "root": str(root),
        "summary": summary,
        "gates": [asdict(g) for g in gates],
    }

    (out_logs / "quality_gates_latest.json").write_text(json.dumps(payload, indent=2), encoding="utf-8")

    md_lines = [
        "# Quality Gates Report",
        f"- generated_at: {payload['generated_at']}",
        f"- root: {payload['root']}",
        "",
        "## Summary",
        f"- PASS: {summary['pass']}",
        f"- WARN: {summary['warn']}",
        f"- FAIL: {summary['fail']}",
        "",
        "## Gates",
    ]
    for g in gates:
        md_lines.append(f"- [{g.status}] {g.name}: {g.detail}")
    (out_logs / "quality_gates_latest.md").write_text("\n".join(md_lines) + "\n", encoding="utf-8")

    print(json.dumps(summary))
    raise SystemExit(2 if summary["fail"] else 0)


if __name__ == "__main__":
    main()
