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

df_train %>%
  ggplot(aes(SalePrice)) +
  geom_density()

n_train <- nrow(df_train)
df <- bind_rows(df_train, df_test) %>%
  rename(FirstFlrSF = `1stFlrSF`, SecondFlrSF = `2ndFlrSF`)



lapply(df, function(x){sum(is.na(x))})

summary(df_train)

factor_plot(df, "PoolQC", "SalePrice", stat = "box", ylim = ylim)
factor_plot(df, "PoolQC", "SalePrice", stat = "count")
factor_plot(df, "Street", "SalePrice", stat="box", ylim = ylim)
factor_plot(df, "Neighborhood", "SalePrice", stat="box", ylim = ylim) + coord_flip()
factor_plot(df, "Neighborhood", "SalePrice", stat="count", ylim = ylim) + coord_flip()

automate_factor_plot(df, "SalePrice", ylim = ylim)


df$MSZoning <- recode(df$MSZoning, .missing = "RL")
df$Alley <- recode(df$Alley, .missing = "None")
df$BsmtQual <- recode(df$BsmtQual, .missing = "None")
df$BsmtCond <- recode(df$BsmtCond, .missing = "None")
df$BsmtExposure <- recode(df$BsmtExposure, .missing = "None")
df$BsmtFinType1 <- recode(df$BsmtFinType1, .missing = "None")
df$BsmtFinType2 <- recode(df$BsmtFinType2, .missing = "None")
df$Electrical <- recode(df$Electrical, .missing = "SBrkr")
df$KitchenQual <- recode(df$KitchenQual, .missing = "Fa")
df$Functional <- recode(df$Functional, .missing = "Typ")
df$FireplaceQu <- recode(df$FireplaceQu, .missing = "None")
df$GarageType <- recode(df$GarageType, .missing = "None")
df$GarageFinish <- recode(df$GarageFinish, .missing = "None")
df$GarageQual <- recode(df$GarageQual, .missing = "None")
df$GarageCond <- recode(df$GarageCond, .missing = "None")
df$Fence <- recode(df$Fence, .missing = "None")
df$MiscFeature <- recode(df$MiscFeature, .missing = "None")
df$SaleType <- recode(df$SaleType, .missing = "WD")


# drop Utilities, PoolQC
# count LandSlope
# ExterQual very important
# CentralAir two-level factor
df$PoolQC <- NULL
df$Utilities <- NULL

sapply(df, function(x){sum(is.na(x))})



