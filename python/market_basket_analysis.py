"""Market Basket Analysis, Python twin (pandas + mlxtend).

This script reproduces the project end to end from the committed dataset
(`Datasets/dataset_bd3.csv`) without any database connection. It exists for
two reasons:

1. It runs anywhere with Python (the original analysis is in R, which is not
   always installed), so the methodology is reproducible and testable.
2. It fixes the transaction-construction bug present in the original R script.
   The R code used `split(Item01, Item02, ...)`, which groups first-purchase
   items by the value of the second item. That collapses ~5,000 purchase rows
   into 117 artificial groups (one per distinct Item02 value), so the mined
   rules describe item co-occurrence inside those groups, not real baskets.
   Here each purchase row is one basket built from Item01..Item06.

The script can run in two modes:

    python market_basket_analysis.py            # corrected (default)
    python market_basket_analysis.py --legacy   # reproduce the v1 split bug

Run from the repository root. Outputs go to `python/output/`.
"""
from __future__ import annotations

import argparse
from pathlib import Path

import pandas as pd
from mlxtend.frequent_patterns import apriori, association_rules
from mlxtend.preprocessing import TransactionEncoder

REPO_ROOT = Path(__file__).resolve().parent.parent
DATA_PATH = REPO_ROOT / "Datasets" / "dataset_bd3.csv"
ITEM_COLS = [f"Item0{i}" for i in range(1, 7)]
TARGETS = [
    "Dust-Off Compressed Gas 2 pack",
    "HP 61 ink",
    "VIVO Dual LCD Monitor Desk mount",
]


def load_dataset(path: Path = DATA_PATH) -> pd.DataFrame:
    """Read the raw CSV as strings, keeping blank cells as empty strings."""
    return pd.read_csv(path, dtype=str, keep_default_na=False)


def drop_blank_rows(raw: pd.DataFrame) -> pd.DataFrame:
    """Keep rows that have at least one non-empty item.

    The raw file alternates a blank row and a data row. The original R code
    kept even-numbered rows by position, which only works if the database
    returns rows in insertion order. Filtering by content is order independent
    and gives the same data rows.
    """
    mask = raw.apply(lambda r: any(str(v).strip() for v in r), axis=1)
    return raw[mask].reset_index(drop=True)


def baskets_corrected(rows: pd.DataFrame) -> list[list[str]]:
    """One basket per purchase row, from the first six item columns."""
    baskets = []
    for _, row in rows[ITEM_COLS].iterrows():
        items = sorted({str(v).strip() for v in row if str(v).strip()})
        if items:
            baskets.append(items)
    return baskets


def baskets_legacy(rows: pd.DataFrame) -> list[list[str]]:
    """Reproduce the original R bug: split(Item01, Item02).

    Groups distinct Item01 values by their Item02 value. Produces 117 groups
    (one per distinct Item02), matching the stored apriori log of
    '104 item(s), 117 transaction(s)'.
    """
    rows = rows[rows["Item02"].str.strip() != ""]
    groups: dict[str, set[str]] = {}
    for _, row in rows.iterrows():
        key = row["Item02"].strip()
        val = row["Item01"].strip()
        groups.setdefault(key, set())
        if val:
            groups[key].add(val)
    return [sorted(s) for s in groups.values()]


def encode(baskets: list[list[str]]) -> pd.DataFrame:
    encoder = TransactionEncoder()
    arr = encoder.fit_transform(baskets)
    return pd.DataFrame(arr, columns=encoder.columns_)


def rules_for_target(
    tdf: pd.DataFrame,
    target: str,
    min_support: float,
    min_confidence: float,
) -> pd.DataFrame:
    """Mine association rules whose right-hand side is exactly `target`."""
    frequent = apriori(tdf, min_support=min_support, use_colnames=True)
    if frequent.empty:
        return pd.DataFrame()
    rules = association_rules(
        frequent, metric="confidence", min_threshold=min_confidence
    )
    rules = rules[rules["consequents"].apply(lambda c: set(c) == {target})]
    if rules.empty:
        return rules
    n = len(tdf)
    rules = rules.assign(count=(rules["support"] * n).round().astype(int))
    cols = ["antecedents", "consequents", "support", "confidence", "lift", "count"]
    return rules[cols].sort_values(["confidence", "lift"], ascending=False)


def format_rules(rules: pd.DataFrame) -> pd.DataFrame:
    out = rules.copy()
    out["rule"] = out.apply(
        lambda x: "{%s} => {%s}"
        % (", ".join(sorted(x["antecedents"])), ", ".join(sorted(x["consequents"]))),
        axis=1,
    )
    return out[["rule", "support", "confidence", "lift", "count"]].reset_index(drop=True)


def run(legacy: bool = False) -> dict[str, pd.DataFrame]:
    raw = load_dataset()
    rows = drop_blank_rows(raw)
    baskets = baskets_legacy(rows) if legacy else baskets_corrected(rows)

    n_items = len({i for b in baskets for i in b})
    print(f"mode: {'legacy (v1 split bug)' if legacy else 'corrected (per-row baskets)'}")
    print(f"transactions: {len(baskets)}  distinct items: {n_items}")

    # Legacy v1 used supp=0.2; corrected per-row baskets are sparser so a lower
    # floor surfaces meaningful rules. Confidence floor matches the R script.
    min_support = 0.2 if legacy else 0.01
    min_confidence = 0.5 if legacy else 0.1

    out_dir = REPO_ROOT / "python" / "output"
    out_dir.mkdir(parents=True, exist_ok=True)

    tdf = encode(baskets)
    results: dict[str, pd.DataFrame] = {}
    for i, target in enumerate(TARGETS, start=1):
        rules = rules_for_target(tdf, target, min_support, min_confidence)
        results[target] = rules
        formatted = format_rules(rules) if not rules.empty else pd.DataFrame()
        suffix = "legacy" if legacy else "corrected"
        path = out_dir / f"df_product{i}_{suffix}.csv"
        formatted.to_csv(path, index=False)
        print(f"\nTarget: {target}")
        print(f"  rules: {len(rules)}  written: {path.relative_to(REPO_ROOT)}")
        if not formatted.empty:
            print(formatted.head(3).to_string(index=False))
    return results


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--legacy",
        action="store_true",
        help="reproduce the original R split() bug instead of the fix",
    )
    args = parser.parse_args()
    run(legacy=args.legacy)


if __name__ == "__main__":
    main()
