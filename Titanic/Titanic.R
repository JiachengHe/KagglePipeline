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

source("Titanic_FE.R")

df <- df %>%
  select(-Name, -Ticket, -Cabin, -Surname, -FamilyID) %>%
  factorize(list("Pclass", "Sex", "Embarked", "Cabin_letter", "Title", "FamilySize", "SibSp", "Parch"))

sapply(df, function(x){sum(is.na(x))})
df <- random_impute(df, yVar = "Survived")



## Feature Engineering
automate_factor_plot(df, yVar = "Survived")

automate_numeric_plot(df, yVar = "Survived")



df <- automate_factor_to_lmfit(df, yVar = "Survived", drop_nzv = FALSE)


X_train <- df %>%
  filter(train_or_test == "train") %>%
  select(-PassengerId, -train_or_test, -Survived) %>%
  as.matrix()

X_test <- df %>%
  filter(train_or_test == "test") %>%
  select(-PassengerId, -train_or_test, -Survived) %>%
  as.matrix()

y_train <- filter(df, train_or_test == "train")$Survived


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





xgb_model <- train(X_train, factor(y_train, label = c("N", "Y")), method = "xgbTree",
                   trControl = trControl, tuneLength = 100)

glmnet_model <- train(X_train, factor(y_train, label = c("N", "Y")), method = "glmnet",
                      preProcess = c("center", "scale"), trControl = trControl, tuneLength = 100)

rf_model <- train(X_train, factor(y_train, label = c("N", "Y")), method = "rf",
                  trControl = trControl, tuneLength = 100)

model_blend <- blending(list(xgb=xgb_model, glmnet=glmnet_model, rf=rf_model), X_train, y_train, X_test,
                        method="glmnet", tuneLength = 100)

save_submission(data_frame(PassengerId = df_test$PassengerId,
                           Survived = as.integer(model_blend$y_pred) - 1))


stopCluster(cl)
