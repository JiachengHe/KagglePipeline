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

df <- bind_rows(mutate(df_train, train_or_test = "train"),
                mutate(df_test, train_or_test = "test")) %>%
  rename(FirstFlrSF = `1stFlrSF`, SecondFlrSF = `2ndFlrSF`, ThreeSsnPorch = `3SsnPorch`) %>%   # Avoid errors
  mutate(MSSubClass = as.character(MSSubClass)) %>%
  random_impute(yVar = "SalePrice")

# lapply(df, function(x){sum(is.na(x))})
# summary(df_train)

factor_plot(df, "PoolQC", "SalePrice", type = "box", ylim = ylim)
factor_plot(df, "PoolQC", "SalePrice", type = "count")
factor_plot(df, "Street", "SalePrice", type="box", ylim = ylim)
factor_plot(df, "Neighborhood", "SalePrice", type="box", ylim = ylim) + coord_flip()
factor_plot(df, "Neighborhood", "SalePrice", type="count", ylim = ylim) + coord_flip()

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

df %>% select_if(is.numeric) %>% sapply(function(x){sum(is.na(x))})

numeric_plot(df, "TotalBsmtSF", "SalePrice", type = "scatter")

automate_numeric_plot(df, "SalePrice")

df <- df %>%        ## delete 7 observations
  filter(!(((LotFrontage > 300) | (LotArea > 100000) | (MasVnrArea > 1500) | (BsmtFinSF1 > 5000) | (TotalBsmtSF > 6000) |
         ((FirstFlrSF>4000) & (FirstFlrSF<5000)) | (GrLivArea > 5500)) & (train_or_test == "train")))

summary(df_train$GarageYrBlt)

