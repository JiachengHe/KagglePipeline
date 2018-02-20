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
                mutate(df_test, train_or_test = "test")) %>%
  rename(FirstFlrSF = `1stFlrSF`, SecondFlrSF = `2ndFlrSF`, ThreeSsnPorch = `3SsnPorch`) %>%   # Avoid errors
  mutate(MSSubClass = as.character(MSSubClass))

factor_plot(df, "MSSubClass", "SalePrice", type = "count")
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
df$MasVnrType <- recode(df$MasVnrType, .missing = "None")
# drop Utilities, PoolQC
# count LandSlope
# ExterQual very important
# CentralAir two-level factor
df$PoolQC <- NULL
df$Utilities <- NULL
df$Street <- NULL
df$Condition2 <- NULL
df$RoofMatl <- NULL
df$Heating <- NULL
df$MiscFeature <- NULL
# Now only "Exterior1st" and "Exterior2nd" have NA

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

df <- factorize(df)


nzv <- nearZeroVar(df, freqCut = 95/5, saveMetrics = TRUE)
rownames(nzv[nzv$nzv,])


df_MSSubClass <- df %>%
  group_by(MSSubClass, train_or_test) %>%
  summarize(SalePrice=mean(SalePrice, na.rm = TRUE), n = n())

df$MSSubClass[df$MSSubClass %in% c(150, 180, 190, 40, 45, 75, 85)] <- "Others"
df$MSZoning[df$MSZoning %in% c("C (all)", "FV", "RH")] <- "Others"
df$Alley[df$Alley %in% c("Grvl", "Pave")] <- "Yes"
df$LotShape[df$LotShape %in% c("IR1", "IR2", "IR3")] <- "IR"
df$LandContour[df$LandContour %in% c("Bnk", "HLS", "Low")] <- "Others"
df$LotConfig[df$LotConfig %in% c("CulDSac", "FR2", "FR3")] <- "Others"
df$LandSlope[df$LandSlope %in% c("Mod", "Sev")] <- "Others"
df$Condition1[df$Condition1 %in% c("Artery", "Feedr", "RRAe")] <- "Low"
df$Condition1[!df$Condition1 %in% c("Low", "Norm")] <- "High"
df$BldgType[df$BldgType %in% c("2fmCon", "Duplex", "Twnhs")] <- "Others"
df$HouseStyle[df$HouseStyle %in% c("1.5Unf", "2.5Fin", "2.5Unf", "SFoyer", "SLvl")] <- "Others"
df$RoofStyle[!df$RoofStyle %in% "Gable"] <- "HipOthers"
# Exterior1st have too many levels
df$Exterior1st[!df$Exterior1st %in% c("HdBoard", "MetalSd", "VinylSd", "Wd Sdng")] <- "Others"
df$Exterior2nd[!df$Exterior2nd %in% c("HdBoard", "MetalSd", "VinylSd", "Wd Sdng")] <- "Others"
df$MasVnrType[df$MasVnrType %in% c("BrkCmn", "Stone")] <- "Others"
df$ExterCond[df$ExterCond %in% c("Ex", "Gd")] <- "High"
df$ExterCond[!df$ExterCond == "High"] <- "Low"
df$Foundation[!df$Foundation %in% c("CBlock", "PConc")] <- "Others"
df$BsmtQual[df$BsmtQual %in% c("Fa", "None")] <- "FaNone"
df$BsmtCond[df$BsmtCond %in% c("Gd", "TA")] <- "Good"
df$BsmtCond[!df$BsmtCond == "Good"] <- "Bad"
df$BsmtExposure[df$BsmtExposure == "None"] <- "No"
df$BsmtFinType1[df$BsmtFinType1 %in% c("LwQ", "None")] <- "Others"
df$BsmtFinType2[!df$BsmtFinType2 == "Unf"] <- "Others"
df$HeatingQC[df$HeatingQC %in% c("Ex", "Gd")] <- "Good"
df$HeatingQC[!df$HeatingQC == "Good"] <- "Bad"
df$Electrical[df$Electrical != "SBrkr"] <- "Others"
df$KitchenQual[df$KitchenQual %in% c("Fa", "TA")] <- "Bad"
df$Functional[df$Functional != "Typ"] <- "Others"
df$FireplaceQu[df$FireplaceQu %in% c("Ex", "Gd")] <- "Good"
df$FireplaceQu[df$FireplaceQu %in% c("Fa", "Po", "TA")] <- "Bad"
df$GarageType[df$GarageType %in% c("2Types", "Basment", "CarPort", "None")] <- "Others"
df$GarageQual[df$GarageQual %in% c("Ex", "Gd", "TA")] <- "Good"
df$GarageQual[df$GarageQual != "Good"] <- "Bad"
df$GarageCond[df$GarageCond %in% c("Ex", "Gd", "TA")] <- "Good"
df$GarageCond[df$GarageCond != "Good"] <- "Bad"
df$PavedDrive[df$PavedDrive != "Y"] <- "Others"
df$Fence[df$Fence %in% c("GdPrv", "GdWo", "MnWw")] <- "Others"
df$SaleType[!df$SaleType %in% c("WD", "New")] <- "Others"
df$SaleCondition[!df$SaleCondition %in% c("Normal", "Partial")] <- "Others"
