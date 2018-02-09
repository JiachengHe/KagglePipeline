devtools::load_all()
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(caret)

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

## Feature Engineering
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

df$familySize <- df$Parch + df$SibSp

df <- factorize(df, list("Sex", "Pclass", "Cabin_letter", "Embarked"))
str(df)

df <- factor_to_lmfit(df, Sex, Survived, trainIndex=1:n_train)
df <- factor_to_lmfit(df, Pclass, Survived, trainIndex=1:n_train)
df <- factor_to_lmfit(df, Embarked, Survived, trainIndex=1:n_train)
df <- factor_to_lmfit(df, Cabin_letter, Survived, trainIndex=1:n_train)

X_train <-
  df[1:n_train, ] %>%
  select(Age, SibSp, Parch, familySize, Fare, Sex_lmfit, Pclass_lmfit, Embarked_lmfit, Cabin_letter_lmfit) %>%
  as.matrix()

X_test <-
  df[-(1:n_train), ] %>%
  select(Age, SibSp, Parch, familySize, Fare, Sex_lmfit, Pclass_lmfit, Embarked_lmfit, Cabin_letter_lmfit) %>%
  as.matrix()

y_train <- df$Survived[1:n_train]

xgb_model <- train(X_train, factor(y_train, label = c("N", "Y")), method = "xgbTree",
                   trControl = trainControl(method = "cv", number = 5, returnData = FALSE,
                                            savePredictions = "final", classProbs = TRUE),
                   tuneGrid = expand.grid(nrounds = 1000, max_depth = 6, eta = 0.3, gamma = 0,
                                          colsample_bytree = 1, min_child_weight = 1, subsample = 1))

submission <-
  data_frame(PassengerId = df_test$PassengerId,
             Survived = predict(xgb_model, X_test))

submission$Survived <- as.integer(submission$Survived) - 1

write.csv(submission, file = "submission_3.csv", row.names = FALSE)

