# Retail Market Basket Analysis

Mining association rules from retail purchase histories to learn which products
are bought together, so a store can stock and recommend them better.

[Executive report (PDF)](https://github.com/Caio-Felice-Cunha/MarketBasketAnalysis/blob/main/Executive%20Report%20-%20Market%20Basket%20Analysis.pdf) ·
[Full report (PDF)](https://github.com/Caio-Felice-Cunha/MarketBasketAnalysis/blob/main/Market-Basket-Analysis-Report.pdf)

![banner](https://user-images.githubusercontent.com/111542025/226082263-2a7f3a81-22ae-4ec2-86f9-940b1dd6639a.jpeg)

## Business problem

Retailers (and recommendation-driven companies such as streaming services) want
to understand customer buying patterns: once someone buys product A, what are
they likely to buy next? Market basket analysis answers this by mining
association rules from transaction data. Each rule has the form
`{items already in basket} => {likely next item}`, scored by support (how often
the combination appears), confidence (how reliably the left side predicts the
right side), and lift (how much more likely than chance).

## Data

The dataset is committed at [`Datasets/dataset_bd3.csv`](Datasets/dataset_bd3.csv):
7,500 purchase records across 20 item columns (`Item01` to `Item20`). The raw
file alternates a blank row and a data row, so the loader keeps only rows that
contain at least one item. After that filtering there are 7,501 purchase rows.
The data and the analysis come from the Data Science Academy "Big Data Analytics
with R and Microsoft Azure Machine Learning" course (see Disclaimer).

The original write-up linked the arules CRAN package page as the data source.
That link is the R library used for mining, not where the data came from. The
real source is the committed CSV described above.

## How to run

### Python twin (recommended, runs anywhere)

A Python port under [`python/`](python/) reproduces the analysis with pandas and
mlxtend and ships a test suite. From the repository root:

```bash
python -m venv .venv
source .venv/bin/activate        # on Windows: .venv\Scripts\activate
pip install -r python/requirements.txt

# Corrected analysis (one basket per purchase row)
python python/market_basket_analysis.py

# Reproduce the original v1 method for comparison
python python/market_basket_analysis.py --legacy

# Run the tests
pytest python/
```

Rule tables are written to `python/output/`.

### R (original)

Install the packages and run the script. It reads the committed CSV, so no
database is needed:

```r
install.packages(c("dplyr", "arules", "arulesViz", "writexl"))
source("Market Basket Analysis.R")
```

An optional MySQL/MariaDB path is included as a commented block that reads
credentials from environment variables (`MBA_DB_USER`, `MBA_DB_PASS`,
`MBA_DB_NAME`, `MBA_DB_HOST`). See `.env.example`.

## Method

1. Load the purchase records and drop the blank separator rows.
2. Build one transaction per purchase row from the first six item columns.
3. Mine association rules with the apriori algorithm, targeting three products
   of interest as the rule consequent (right-hand side): Dust-Off Compressed Gas
   2 pack, HP 61 ink, and VIVO Dual LCD Monitor Desk mount.
4. Remove redundant rules and rank by confidence and lift.

These three products were chosen as targets to study, not discovered as the
top sellers. For reference, the most frequent single items in the corrected
baskets are Dust-Off Compressed Gas 2 pack (23.5% of baskets), VIVO Dual LCD
Monitor Desk mount (17.3%), Apple Pencil (16.3%), and HP 61 ink (14.9%).

## Results

### Corrected method (v2)

Building one basket per purchase row gives 7,501 transactions over 119 distinct
items, average basket size 3.4. Strongest rules per target product
(support >= 0.01, ranked by lift then confidence), reproduced by
`python/market_basket_analysis.py`:

| Target | Top rule | Support | Confidence | Lift |
|---|---|---|---|---|
| Dust-Off Compressed Gas 2 pack | {SanDisk Ultra 64GB card, VIVO Dual LCD Monitor Desk mount} => Dust-Off | 0.0157 | 0.413 | 1.75 |
| HP 61 ink | {Dust-Off Compressed Gas 2 pack, VIVO Dual LCD Monitor Desk mount} => HP 61 ink | 0.0120 | 0.209 | 1.40 |
| VIVO Dual LCD Monitor Desk mount | {Dust-Off Compressed Gas 2 pack, SanDisk Ultra 64GB card} => VIVO mount | 0.0157 | 0.400 | 2.32 |

Lift above 1 means the products co-occur more than chance would predict, so the
left-side items are useful signals for recommending the target.

### Original method (v1) and the bug behind it

The first version (the two committed PDFs and the `Datasets/df_product*.xlsx`
files) used `split(Item01, Item02, ...)` to build transactions. R's `split()`
accepts only one grouping factor, so items 3 through 6 were silently dropped and
first-purchase items were grouped by the value of the second item. That collapses
roughly 5,000 purchase rows into 117 artificial groups, exactly the
"104 item(s), 117 transaction(s)" line in the apriori logs (117 is the number of
distinct `Item02` values). The headline v1 rules, faithfully reproduced by
`market_basket_analysis.py --legacy`:

| Rule | Support | Confidence | Lift | Count |
|---|---|---|---|---|
| {Screen Mom Screen Cleaner kit, VIVO Dual LCD Monitor Desk mount} => Dust-Off | 0.299 | 1.000 | 1.746 | 35 |
| {Apple Lightning to Digital AV Adapter, SAMSUNG EVO 32GB card} => HP 61 ink | 0.248 | 0.879 | 1.869 | 29 |
| {Dust-Off Compressed Gas 2 pack, SanDisk Ultra 64GB card} => VIVO mount | 0.231 | 0.964 | 2.129 | 27 |

After redundancy filtering the v1 run produced 27, 30, and 30 rules for the three
targets, matching the row counts in the committed xlsx files. The v1 numbers are
internally consistent but rest on the grouped 117-transaction structure rather
than real baskets, which is why the corrected v2 numbers above use much lower
support. Treat the PDFs as the v1 artifact.

## What changed in this revision

- Fixed the transaction construction so each purchase row is one basket and items
  3 to 6 are no longer dropped.
- Replaced the broken MySQL connection (it referenced undefined variables and
  errored on load) with a direct read of the committed CSV; kept the database
  path as an optional, env-var-based commented block.
- Switched blank-row removal from position-based to content-based filtering.
- Added a Python twin with a passing test suite that pins the dataset shape and
  the exact stored rule metrics.
- Added `.gitignore`, `python/requirements.txt`, and `.env.example`.

## Next steps

- Tune support and confidence thresholds per product to balance coverage and
  precision.
- Mine rules across all items rather than three fixed targets.
- Regenerate the PDF reports from the corrected v2 pipeline.

## Disclaimer

A good part of this project was originally done in the Data Science Academy
"Big Data Analytics with R and Microsoft Azure Machine Learning" course, part of
the Data Scientist training.
