devtools::load_all()
library(readr)
library(dplyr)
library(ggplot2)
library(caret)

options(readr.num_columns = 0)
df_train <- read_csv("House_Prices/data/train.csv")
df_test <- read_csv("House_Prices/data/test.csv")

df_train$SalePrice <- log(df_train$SalePrice)
ylim <- c(min(df_train$SalePrice, na.rm = TRUE), max(df_train$SalePrice, na.rm = TRUE))

n_train <- nrow(df_train)
n_test <- nrow(df_test)

df <- bind_rows(mutate(df_train, train_or_test = "train"),
                mutate(df_test, train_or_test = "test"))



factor_plot(df, "Neighborhood", "SalePrice", type="box", ylim = ylim) + coord_flip()
factor_plot(df, "Neighborhood", "SalePrice", type="count", ylim = ylim) + coord_flip()

automate_factor_plot(df, "SalePrice", ylim = ylim)




is_cat <- function(x) {is.factor(x) | is.character(x)}
sapply(select_if(df, is_cat), function(x){sum(is.na(x))})
sapply(select_if(df, is.numeric), function(x){sum(is.na(x))})
sapply(df, function(x){sum(is.na(x))})
df <- random_impute(df, yVar = "SalePrice")


numeric_plot(df, "TotalBsmtSF", "SalePrice", type = "scatter")

automate_numeric_plot(df, "SalePrice")

df <- df %>%        ## delete 7 observations
  filter(!(((LotFrontage > 300) | (LotArea > 100000) | (MasVnrArea > 1500) | (BsmtFinSF1 > 5000) | (TotalBsmtSF > 6000) |
         ((FirstFlrSF>4000) & (FirstFlrSF<5000)) | (GrLivArea > 5500)) & (train_or_test == "train")))

sum(df$train_or_test == "test") # Make sure no observations in the test are removed




nzv <- nearZeroVar(df, freqCut = 95/5, saveMetrics = TRUE)
rownames(nzv[nzv$nzv,])
sapply(rownames(nzv[nzv$nzv,]), function(x){table(df[[x]])})
sapply(rownames(nzv[nzv$nzv,]), function(x){is.numeric(df[[x]])})

source("House_Prices/House_Prices_FE.R")
df <- automate_factor_to_lmfit(df, "SalePrice")
