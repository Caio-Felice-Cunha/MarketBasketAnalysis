
# https://cran.r-project.org/web/packages/arules/index.html
# https://cran.r-project.org/web/packages/arulesViz/index.html

# Loading Packages
library(dplyr)
library(arules) # Package used for association rules
library(arulesViz) # Package aimed at visualizing association rules
library(writexl)

# Read the committed dataset directly. No database is required to reproduce
# this analysis; the CSV under Datasets/ is the source of truth.
data <- read.csv("Datasets/dataset_bd3.csv", stringsAsFactors = FALSE)

# Optional MySQL path. Credentials come from environment variables so nothing
# is committed. Uncomment and set MBA_DB_USER / MBA_DB_PASS / MBA_DB_NAME /
# MBA_DB_HOST to read from a live database instead of the CSV.
# library(RMariaDB)  # RMySQL is legacy; RMariaDB is the maintained successor.
# con <- dbConnect(
#   RMariaDB::MariaDB(),
#   user     = Sys.getenv("MBA_DB_USER"),
#   password = Sys.getenv("MBA_DB_PASS"),
#   dbname   = Sys.getenv("MBA_DB_NAME"),
#   host     = Sys.getenv("MBA_DB_HOST")
# )
# data <- dbGetQuery(con, "SELECT * FROM dataset_bd3")
# dbDisconnect(con)

# Explore the dataset
dim(data)
head(data)
str(data)

# The raw file alternates a blank row and a data row. Keep rows that have at
# least one non-empty item instead of selecting by position. Position-based
# filtering (even rows) only works if the source returns rows in insertion
# order, which SQL does not guarantee without ORDER BY.
non_blank <- rowSums(nchar(trimws(as.matrix(data))) > 0) > 0
df1 <- data[non_blank, ]
head(df1)

# Number of distinct purchase rows
n_distinct(df1)

# Work only with records where the second item is present (rules need >= 2 items)
df1_two <- df1[!grepl("^\\s*$", df1$Item02), ]
n_distinct(df1_two)

# Build one transaction per purchase row from the first six item columns.
#
# The original version used split(Item01, Item02, ...). R's split() takes a
# single grouping factor, so Item03..Item06 were silently consumed as other
# arguments and the result grouped first-purchase items by the value of the
# second item. That collapsed thousands of purchase rows into 117 artificial
# groups (one per distinct Item02 value) and ignored items 3 through 6. The
# code below builds a real basket per row instead.
item_cols <- paste0("Item0", 1:6)
basket_list <- apply(df1_two[, item_cols], 1, function(r) {
  r <- trimws(r)
  unique(r[nchar(r) > 0])
})
transactions <- as(basket_list, "transactions")

# Inspect a few transactions
inspect(head(transactions, 5))
summary(transactions)

# Rules for a selected product: Dust-Off Compressed Gas 2 pack
# (These three products were chosen as targets of interest; the analysis
# confirms association rules that lead to them, it does not rank them as the
# most purchased items.)
product_rules1 <- apriori(
  transactions,
  parameter = list(supp = 0.01, conf = 0.1, minlen = 2, target = "rules"),
  appearance = list(rhs = "Dust-Off Compressed Gas 2 pack", default = "lhs")
)
product_rules1_clean <- product_rules1[!is.redundant(product_rules1)]
inspect(head(sort(product_rules1_clean, by = "confidence"), 5))
summary(product_rules1_clean)

# Rules for a selected product: HP 61 ink
product_rules2 <- apriori(
  transactions,
  parameter = list(supp = 0.01, conf = 0.1, minlen = 2, target = "rules"),
  appearance = list(rhs = "HP 61 ink", default = "lhs")
)
product_rules2_clean <- product_rules2[!is.redundant(product_rules2)]
inspect(head(sort(product_rules2_clean, by = "confidence"), 5))
summary(product_rules2_clean)

# Rules for a selected product: VIVO Dual LCD Monitor Desk mount
product_rules3 <- apriori(
  transactions,
  parameter = list(supp = 0.01, conf = 0.1, minlen = 2, target = "rules"),
  appearance = list(rhs = "VIVO Dual LCD Monitor Desk mount", default = "lhs")
)
product_rules3_clean <- product_rules3[!is.redundant(product_rules3)]
inspect(head(sort(product_rules3_clean, by = "confidence"), 5))
summary(product_rules3_clean)

# Save each ruleset as a data frame and write to disk
df_product1 <- as(product_rules1_clean, "data.frame")
write_xlsx(df_product1, "Datasets/df_product1.xlsx")

df_product2 <- as(product_rules2_clean, "data.frame")
write_xlsx(df_product2, "Datasets/df_product2.xlsx")

df_product3 <- as(product_rules3_clean, "data.frame")
write_xlsx(df_product3, "Datasets/df_product3.xlsx")

## Disclaimer: a good part of this project was largely done in the Data Science
## Academy, Big Data Analytics with R and Microsoft Azure Machine Learning
## course (part of the Data Scientist training)
