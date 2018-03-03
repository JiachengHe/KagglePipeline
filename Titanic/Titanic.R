devtools::load_all()
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(caret)
library(doParallel)

options(readr.num_columns = 0)
df_train <- read_csv("data/train.csv")
df_test <- read_csv("data/test.csv")


n_train <- nrow(df_train)
n_test <- nrow(df_test)

df <- bind_rows(mutate(df_train, train_or_test = "train"),
                mutate(df_test, train_or_test = "test"))
df$Survived <- factor(df$Survived)


sapply(df, function(x){sum(is.na(x))})

# numeric_plot(df, "Age", "Survived")
numeric_plot(df, "Fare", "Survived", ylim = c(0,300))
numeric_plot(df, "Age", "Survived")
factor_plot(df, "Embarked", "Survived")

source("Titanic_FE.R")

df <- df %>%
  select(-Name, -Ticket, -Cabin, -Surname, -Surname) %>%
  factorize(list("Pclass", "Sex", "Embarked", "Cabin_letter", "Title", "FamilyID"))

df <- random_impute(df, yVar = "Survived")



## Feature Engineering
automate_factor_plot(df, yVar = "Survived")

automate_numeric_plot(df, yVar = "Survived")



df <- automate_factor_to_lmfit(df, yVar = "Survived", drop_nzv = FALSE)


X_train <-
  df[1:n_train, ] %>%
  select(Age, SibSp, Parch, FamilySize, Fare, Sex_lmfit, Pclass_lmfit, Embarked_lmfit, Cabin_letter_lmfit, Title_lmfit, FamilyID_lmfit) %>%
  as.matrix()

X_test <-
  df[-(1:n_train), ] %>%
  select(Age, SibSp, Parch, FamilySize, Fare, Sex_lmfit, Pclass_lmfit, Embarked_lmfit, Cabin_letter_lmfit, Title_lmfit, FamilyID_lmfit) %>%
  as.matrix()

y_train <- df$Survived[1:n_train]


trControl <- trainControl(method = "cv", number = 5, returnData = FALSE,
                            savePredictions = "final", classProbs = TRUE, search = "random")


num_cores <- detectCores(logical = FALSE) - 1
cl <- makeCluster(num_cores, type = "SOCK")
registerDoParallel(cl)

xgb_model <- train(X_train, factor(y_train, label = c("N", "Y")), method = "xgbTree",
                   trControl = trControl, tuneLength = 100)
write_models_log(xgb_model, "xgb2")
save_submission(data_frame(PassengerId = df_test$PassengerId,
                           Survived = as.integer(predict(xgb_model, X_test)) - 1))

glmnet_model <- train(X_train, factor(y_train, label = c("N", "Y")), method = "glmnet",
                      preProcess = c("center", "scale"), trControl = trControl, tuneLength = 50)
write_models_log(glmnet_model, "glmnet")
save_submission(data_frame(PassengerId = df_test$PassengerId,
                           Survived = as.integer(predict(glmnet_model, X_test)) - 1))

rf_model <- train(X_train, factor(y_train, label = c("N", "Y")), method = "rf",
                  trControl = trControl, tuneLength = 50)
write_models_log(rf_model, "rf")
save_submission(data_frame(PassengerId = df_test$PassengerId,
                           Survived = as.integer(predict(rf_model, X_test)) - 1))


# save(xgb_model, file = paste0("Titanic/model/xgbModel_", ver_sub, ".RData"))

#submission <-
#  data_frame(PassengerId = df_test$PassengerId,
#             Survived = predict(xgb_model, X_test))

#submission$Survived <- as.integer(submission$Survived) - 1


#write.csv(submission, file = paste0("Titanic/submission/submission_", ver_sub, ".csv"), row.names = FALSE)

stopCluster(cl)
