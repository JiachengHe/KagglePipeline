devtools::load_all()
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

df_train <- read_csv("../train.csv")
df_test <- read_csv("../test.csv")
n_train <- dim(df_train)[1]

df <- bind_rows(df_train, df_test)

lapply(df, summary)
# Find variables with NA
lapply(df, function(x){sum(is.na(x))})

df_train$Cabin_letter <- ifelse(is.na(df_train$Cabin), "NA", str_sub(df_train$Cabin, 1, 1))
df <- replace_na(df, list(Cabin = "NA", Embarked = "NA", Fare = median(df$Fare, na.rm = TRUE)))

colnames(df)
