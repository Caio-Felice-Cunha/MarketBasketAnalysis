
# https://cran.r-project.org/web/packages/arules/index.html
# https://cran.r-project.org/web/packages/arulesViz/index.html

# Loading Packages
library(dplyr)
library(arules) #Package used for association rules
library(arulesViz) # Package aimed at visualizing association rules
library(htmlwidgets)
library(writexl)
library(RMySQL)
options(warn=-1)



## Conection MySQL
con = dbConnect(MySQL(), user = "root", password = a, dbname = db, host = h)

qry <- 'select * from dataset_bd3'

# Load and explore the dataset
data <- dbGetQuery(con, qry)
dim(data)
head(data)
summary(data)
str(data)

# A smart way to solve the problem in the dataset
# Separate even lines from odd lines
even_lines <- seq(2, nrow(data), 2)
odd_lines <- seq(1, nrow(data), 2)

# We separate the data and then use the dataset with the even lines (valid data lines)
df1 <- data[even_lines, ]
head(df1)
df2 <- data[odd_lines, ] 
head(df2)


# Check if we have missing values in the first purchase item
sum(is.na(df1$Item01))

# Check if we have missing values in the second purchase item (ATTENTION)
sum(is.na(df1$Item02))
#View(df1)

# Check if we have missing values represented by white space
which(nchar(trimws(df1$Item01))==0)
which(nchar(trimws(df1$Item02))==0)

# Check if we have missing values represented by whitespace (using regular expression)
grepl("^\\s*$", df1$Item02)

# Number of distinct items
n_distinct(df1)

# Let's work only with records where item 2 was not null
df1_two <- df1[!grepl("^\\s*$", df1$Item02), ]

# Number of distinct items
n_distinct(df1_two)

# Prepare the package by converting the variables to factor type
# (variables we will use from now on)
#View(df1_two)
package <- df1_two
package$Item01 <- as.factor(package$Item01)
package$Item02 <- as.factor(package$Item02)
package$Item03 <- as.factor(package$Item03)
package$Item04 <- as.factor(package$Item04)
package$Item05 <- as.factor(package$Item05)
package$Item06 <- as.factor(package$Item06)
# summary(package)
# View(package)
# str(package)

package_split <- split(package$Item01, 
                       package$Item02,
                       package$Item03, 
                       package$Item04,
                       package$Item05, 
                       package$Item06,
                      drop = FALSE)

#View(package_split)

# Transactions
transactions <- as(package_split, "transactions")

# Inspection of the rules
inspect(head(transactions, 5))

# Let's check the rules of a product: Dust-Off Compressed Gas 2 pack
product_rules1 <- apriori(transactions, 
                           parameter = list(conf = 0.5, minlen = 3),
                           appearance = list(rhs = "Dust-Off Compressed Gas 2 pack", default = "lhs")) 

# Inspection of the rules
inspect(head(sort(product_rules1, by = "confidence"), 5))

# Let's check the rules of a product: HP 61 ink
product_rules2 <- apriori(transactions,
                           parameter = list(minlen = 3, conf = 0.5),
                           appearance = list(rhs = "HP 61 ink",default = "lhs"))

# Inspection of the rules
inspect(head(sort(product_rules2, by = "confidence"), 5))

# Let's check the rules of a product: VIVO Dual LCD Monitor Desk mount
product_rules3 <- apriori(transactions,
                           parameter = list(minlen = 3, conf = 0.5),
                           appearance = list(rhs = "VIVO Dual LCD Monitor Desk mount", default = "lhs"))

# Inspection of the rules
inspect(head(sort(product_rules3, by = "confidence"), 5))

# Let's double-check the product rules: Dust-Off Compressed Gas 2 pack,
# changing one of the metrics
product_rules1 <- apriori(transactions, 
                           parameter = list(minlen = 3, supp = 0.2, conf = 0.5, target = "rules"),
                           appearance = list(rhs = "Dust-Off Compressed Gas 2 pack", default = "lhs")) 

# Inspection of the rules
inspect(head(sort(product_rules1, by = "confidence"), 5))

# Filter out redundant rules
product_rules1_clean <- product_rules1[!is.redundant(product_rules1)]

# Inspection of the rules
inspect(head(sort(product_rules1_clean, by = "confidence"), 5))

# Summary
summary(product_rules1_clean)

# Plot
plot(product_rules1_clean, measure = "support", shading = "confidence", method = "graph", engine = "html")

# Let's double-check the product rules: HP 61 ink,
# changing one of the metrics
product_rules2 <- apriori(transactions,
                           parameter = list(minlen = 3, supp = 0.2, conf = 0.5, target = "rules"),
                           appearance = list(rhs = "HP 61 ink", default = "lhs"))

# Inspection of the rules
inspect(head(sort(product_rules2, by = "confidence"), 5))

# Filter out redundant rules
product_rules2_clean <- product_rules2[!is.redundant(product_rules2)]

# Inspection of the rules
inspect(head(sort(product_rules2_clean, by = "confidence"), 5))

# Summary
summary(product_rules2_clean)

# Plot
plot(product_rules2_clean, measure = "support", shading = "confidence", method = "graph", engine = "html")

# Let's double-check the product rules: VIVO Dual LCD Monitor Desk mount,
# changing one of the metrics
product_rules3 <- apriori(transactions,
                           parameter = list(minlen = 3, supp = 0.2, conf = 0.5, target = "rules"),
                           appearance = list(rhs = "VIVO Dual LCD Monitor Desk mount", default = "lhs"))

# Inspection of the rules
inspect(head(sort(product_rules3, by = "confidence"), 5))

# Filter out redundant rules
product_rules3_clean <- product_rules3[!is.redundant(product_rules3)]

# Inspection of the rules
inspect(head(sort(product_rules3_clean, by = "confidence"), 5))

# Summary
summary(product_rules3_clean)

# Plot
plot(product_rules3_clean, measure = "support", shading = "confidence", method = "graph", engine = "html")

# Top 3 rules
inspect(head(sort(product_rules1_clean, by = "support", decreasing = TRUE), 1))
inspect(head(sort(product_rules2_clean, by = "confidence", decreasing = TRUE), 1))
inspect(head(sort(product_rules3_clean, by = "confidence", decreasing = TRUE), 1))

# We save the ruleset of the 3 products as a dataframe and then save it to disk
df_product1 <- as(product_rules1_clean, "data.frame")
head(df_product1)
write_xlsx(df_product1, "df_product1.xlsx")

df_product2 <- as(product_rules2_clean, "data.frame")
head(df_product2)
write_xlsx(df_product2, "df_product2.xlsx")

df_product3 <- as(product_rules3_clean, "data.frame")
head(df_product3)
write_xlsx(df_product3, "df_product3.xlsx")



## Disclaimer: a good part of this project was largely done in the Data Science Academy, Big Data Analytics with R and Microsoft Azure Machine Learning course (part of the Data Scientist training)
