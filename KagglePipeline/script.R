devtools::load_all()
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(caret)
library(Rmpi)
library(doParallel)


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


xgb_control <- trainControl(method = "cv", number = 5, returnData = FALSE,
                            savePredictions = "final", classProbs = TRUE)

xgb_grid <- expand.grid(nrounds = seq(600, 2000, 200), max_depth = 2:6, eta = seq(0.01, 0.1, 0.01), gamma = 0,
                        colsample_bytree = seq(0.5, 1, 0.1), min_child_weight = 1, subsample = seq(0.5, 1, 0.1))



num_nodes <- mpi.universe.size() - 1
cl <- makeCluster(num_nodes, type = "MPI")
registerDoParallel(cl)

xgb_model <- train(X_train, factor(y_train, label = c("N", "Y")), method = "xgbTree",
                   trControl = xgb_control, tuneGrid = xgb_grid)

save(xgb_model, file = "xgbModel.RData")

submission <-
  data_frame(PassengerId = df_test$PassengerId,
             Survived = predict(xgb_model, X_test))

submission$Survived <- as.integer(submission$Survived) - 1

write.csv(submission, file = "submission_4.csv", row.names = FALSE)

