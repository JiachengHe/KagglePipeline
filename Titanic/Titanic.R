devtools::load_all()
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(caret)
library(doParallel)
if (Sys.info()['sysname'] == "Linux") { library(Rmpi) }
if ("Titanic" %in% list.dirs(recursive = FALSE, full.names = FALSE)) { setwd("Titanic") }

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

# automate_factor_plot(df, yVar = "Survived")
# automate_numeric_plot(df, yVar = "Survived")

df <- automate_factor_to_lmfit(df, yVar = "Survived", drop_nzv = FALSE)


X_train <- df %>%
  filter(train_or_test == "train") %>%
  select(-PassengerId, -train_or_test, -Survived) %>%
  as.matrix()

X_test <- df %>%
  filter(train_or_test == "test") %>%
  select(-PassengerId, -train_or_test, -Survived) %>%
  as.matrix()

y_train <- filter(df, train_or_test == "train")$Survived %>%
  factor(label = c("N", "Y"))


trCon <- trainControl(method = "cv", number = 5, returnData = FALSE,
                          savePredictions = "final", classProbs = TRUE, search = "random")
train_ <- purrr::partial(train, x = X_train, y = y_train, trControl = trCon, metric = "Accuracy")

write_submission_ <- function(model) {
  write_submission(data_frame(PassengerId = df_test$PassengerId,
                             Survived = as.integer(predict(model, X_test)) - 1))
}


if (Sys.info()['sysname'] == "Windows") {
  num_cores <- detectCores(logical = FALSE) - 1
  cl <- makeCluster(num_cores, type = "SOCK")
} else if (Sys.info()['sysname'] == "Linux") {
  num_nodes <- mpi.universe.size() - 1
  cl <- makeCluster(num_nodes, type = "MPI")
}

registerDoParallel(cl)


xgb_model <- train_(method = "xgbTree", tuneLength = 100)
write_model(xgb_model, "xgb")
write_submission_(xgb_model)

glmnet_model <- train_(method = "glmnet", preProcess = c("center", "scale"), tuneLength = 100)
write_model(glmnet_model, "glmnet")
write_submission_(glmnet_model)

rf_model <- train_(method = "rf", tuneLength = 11, ntree = 1000)
write_model(rf_model, "rf")
write_submission_(rf_model)



model_blend <- blending(list(xgb=xgb_model, glmnet=glmnet_model, rf=rf_model), X_train, y_train, X_test,
                        method="glmnet", tuneLength = 100)
write_model(model_blend, "blend_xgbT_glmnet_rf")

write_submission(data_frame(PassengerId = df_test$PassengerId,
                           Survived = as.integer(model_blend$y_pred) - 1))


stopCluster(cl)
