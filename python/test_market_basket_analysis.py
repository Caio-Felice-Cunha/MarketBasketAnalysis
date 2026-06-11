"""Tests for the Python twin of the Market Basket Analysis.

These tests pin down both the dataset shape and the two known-good numbers:

* the corrected per-row method yields 7,501 baskets, and
* the legacy split() reproduction yields the stored '104 items, 117
  transactions' and the exact top rule metrics found in the committed reports.

Run from the repository root:  pytest python/
"""
from __future__ import annotations

import math

import pytest

import market_basket_analysis as mba


@pytest.fixture(scope="module")
def rows():
    raw = mba.load_dataset()
    return mba.drop_blank_rows(raw)


def test_dataset_loads_with_twenty_item_columns():
    raw = mba.load_dataset()
    assert raw.shape[1] == 20
    assert list(raw.columns[:3]) == ["Item01", "Item02", "Item03"]


def test_blank_row_filter_keeps_only_data_rows(rows):
    # The raw file alternates blank and data rows; half are kept.
    assert len(rows) == 7501
    # Every kept row has at least one non-empty cell.
    assert rows.apply(lambda r: any(str(v).strip() for v in r), axis=1).all()


def test_corrected_baskets_are_one_per_purchase_row(rows):
    baskets = mba.baskets_corrected(rows)
    assert len(baskets) == 7501
    # Items within a basket are unique and non-empty.
    for b in baskets[:50]:
        assert b == sorted(set(b))
        assert all(item.strip() for item in b)


def test_legacy_reproduces_stored_transaction_counts(rows):
    baskets = mba.baskets_legacy(rows)
    distinct_items = len({i for b in baskets for i in b})
    # Matches the apriori log '104 item(s), 117 transaction(s)'.
    assert len(baskets) == 117
    assert distinct_items == 104


def test_legacy_top_rule_metrics_match_stored_report(rows):
    """The HP 61 ink top rule is quoted in the committed PDF report.

    {Apple Lightning to Digital AV Adapter, SAMSUNG EVO 32GB card} => {HP 61 ink}
    support 0.2478632, confidence 0.8787879, lift 1.869421, count 29
    """
    baskets = mba.baskets_legacy(rows)
    tdf = mba.encode(baskets)
    rules = mba.rules_for_target(
        tdf, "HP 61 ink", min_support=0.2, min_confidence=0.5
    )
    top = rules.sort_values("confidence", ascending=False).iloc[0]
    assert set(top["antecedents"]) == {
        "Apple Lightning to Digital AV Adapter",
        "SAMSUNG EVO 32GB card",
    }
    assert math.isclose(top["support"], 0.2478632, abs_tol=1e-6)
    assert math.isclose(top["confidence"], 0.8787879, abs_tol=1e-6)
    assert math.isclose(top["lift"], 1.869421, abs_tol=1e-5)
    assert int(top["count"]) == 29


def test_legacy_dustoff_stored_rule_matches_report(rows):
    """Dust-Off top rule from the report and df_product1.xlsx.

    {Screen Mom Screen Cleaner kit, VIVO Dual LCD Monitor Desk mount}
        => {Dust-Off Compressed Gas 2 pack}
    support 0.2991453, confidence 1.0, lift 1.746269, count 35.
    """
    baskets = mba.baskets_legacy(rows)
    tdf = mba.encode(baskets)
    rules = mba.rules_for_target(
        tdf, "Dust-Off Compressed Gas 2 pack", min_support=0.2, min_confidence=0.5
    )
    target_lhs = {
        "Screen Mom Screen Cleaner kit",
        "VIVO Dual LCD Monitor Desk mount",
    }
    hit = rules[rules["antecedents"].apply(lambda a: set(a) == target_lhs)]
    assert len(hit) == 1
    row = hit.iloc[0]
    assert math.isclose(row["support"], 0.2991453, abs_tol=1e-6)
    assert math.isclose(row["confidence"], 1.0, abs_tol=1e-9)
    assert math.isclose(row["lift"], 1.746269, abs_tol=1e-5)
    assert int(row["count"]) == 35


def test_corrected_rules_have_single_target_consequent(rows):
    baskets = mba.baskets_corrected(rows)
    tdf = mba.encode(baskets)
    rules = mba.rules_for_target(
        tdf,
        "VIVO Dual LCD Monitor Desk mount",
        min_support=0.01,
        min_confidence=0.1,
    )
    assert len(rules) > 0
    assert rules["consequents"].apply(
        lambda c: set(c) == {"VIVO Dual LCD Monitor Desk mount"}
    ).all()
    # Confidence and lift are valid probabilities/ratios.
    assert (rules["confidence"] >= 0).all() and (rules["confidence"] <= 1).all()
    assert (rules["lift"] > 0).all()
