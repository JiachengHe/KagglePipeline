devtools::load_all()
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

df_train <- read_csv("../train.csv")
df_test <- read_csv("../test.csv")
n_train <- dim(df_train)[1]

df <- bind_rows(df_train, df_test)

lapply(df, summary)
# Find variables with NA
lapply(df, function(x){sum(is.na(x))})

df$Cabin_letter <- ifelse(is.na(df$Cabin), "NA", str_sub(df$Cabin, 1, 1))
df <- replace_na(df, list(Cabin = "NA", Embarked = "NA", Fare = median(df$Fare, na.rm = TRUE)))

table(df$Cabin_letter)


factor_plot(df, Cabin_letter, Survived, stat = "identity")
factor_plot(df, Cabin_letter, Survived, stat = "count")
df$Cabin_letter[df$Cabin_letter == "T"] <- "NA"
df$Cabin_letter[df$Cabin_letter %in% c("B", "D", "E")] <- "high"
df$Cabin_letter[df$Cabin_letter %in% c("A", "C", "F", "G")] <- "low"


factor_plot(df, Embarked, Survived, stat = "identity")
factor_plot(df, Embarked, Survived, stat = "count")
df$Embarked[df$Embarked == "NA"] <- "S"

factor_plot(df, Pclass, Survived, stat = "identity")
factor_plot(df, Pclass, Survived, stat = "count")

factor_plot(df, Sex, Survived, stat = "identity")
factor_plot(df, Sex, Survived, stat = "count")


