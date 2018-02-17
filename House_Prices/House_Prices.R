devtools::load_all()
library(readr)
library(dplyr)
library(ggplot2)

options(readr.num_columns = 0)
df_train <- read_csv("House_Prices/data/train.csv")
df_test <- read_csv("House_Prices/data/test.csv")

n_train <- nrow(df_train)
df <- bind_rows(df_train, df_test)

lapply(df, function(x){sum(is.na(x))})

summary(df_train)

factor_plot(df, "MSZoning", "SalePrice", stat = "identity")
factor_plot(df, "MSZoning", "SalePrice", stat = "count")

factor_plot(df, "Street", "SalePrice", stat="identity")

automate_plotting(df, "SalePrice")

for (i in 1:10) {

  n <- readline(prompt = "Press Enter for next plot")

  print(n)
}

df[,sapply(df, class) %in% c("character", "factor")]


df %>%
  select_("MSZoning", "SalePrice") %>%
  group_by_("MSZoning") %>%
  summarize_("SalePrice = mean(SalePrice, na.rm = TRUE)")
