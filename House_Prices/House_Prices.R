devtools::load_all()
library(readr)
library(dplyr)
library(ggplot2)
library(caret)
library(doParallel)

if ("House_Prices" %in% list.dirs(recursive = FALSE, full.names = FALSE)) {
  setwd("House_Prices")
}
options(readr.num_columns = 0)
df_train <- read_csv("data/train.csv")
df_test <- read_csv("data/test.csv")

df_train$SalePrice <- log(df_train$SalePrice)
ylim <- c(min(df_train$SalePrice, na.rm = TRUE), max(df_train$SalePrice, na.rm = TRUE))

n_train <- nrow(df_train)
n_test <- nrow(df_test)

df <- bind_rows(mutate(df_train, train_or_test = "train"),
                mutate(df_test, train_or_test = "test"))

# factor_plot(df, "Neighborhood", "SalePrice", type="box", ylim = ylim) + coord_flip()
# factor_plot(df, "Neighborhood", "SalePrice", type="count", ylim = ylim) + coord_flip()
# automate_factor_plot(df, "SalePrice", ylim = ylim)

source("House_Prices_FE.R")  # feature engineering codes

df <- automate_factor_to_lmfit(df, "SalePrice", drop_nzv = FALSE)

# is_cat <- function(x) {is.factor(x) | is.character(x)}
# sapply(select_if(df, is_cat), function(x){sum(is.na(x))})
# sapply(select_if(df, is.numeric), function(x){sum(is.na(x))})
sapply(df, function(x){sum(is.na(x))})

df <- random_impute(df, yVar = "SalePrice")


#numeric_plot(df, "TotalBsmtSF", "SalePrice", type = "scatter")
#automate_numeric_plot(df, "SalePrice")

df <- df %>%        ## delete 7 observations
  filter(!(((LotFrontage > 300) | (LotArea > 100000) | (MasVnrArea > 1500) | (BsmtFinSF1 > 5000) | (TotalBsmtSF > 6000) |
         ((FirstFlrSF>4000) & (FirstFlrSF<5000)) | (GrLivArea > 5500)) & (train_or_test == "train")))

sum(df$train_or_test == "test") == n_test    # Make sure no observations in the test are removed




nzv <- nearZeroVar(df, freqCut = 95/5, saveMetrics = TRUE)
rownames(nzv[nzv$nzv,])
#sapply(rownames(nzv[nzv$nzv,]), function(x){table(df[[x]])})
#sapply(rownames(nzv[nzv$nzv,]), function(x){is.numeric(df[[x]])})


X_train <- df %>%
  filter(train_or_test == "train") %>%
  select(-Id, -train_or_test, -SalePrice) %>%
  as.matrix()

X_test <- df %>%
  filter(train_or_test == "test") %>%
  select(-Id, -train_or_test, -SalePrice) %>%
  as.matrix()

y_train <- filter(df, train_or_test == "train")$SalePrice

trCon <- trainControl(method = "cv", number = 5, returnData = FALSE,
                          savePredictions = "final", classProbs = FALSE, search = "random")
train_ <- purrr::partial(train, x = X_train, y = y_train, trControl = trCon, metric = "RMSE")

write_submission_ <- function(model) {
  write_submission(data_frame(Id = df_test$Id,
                              SalePrice = exp(predict(model, X_test))))
}



cl <- makeCluster(detectCores(logical = FALSE) - 1, type = "SOCK")
registerDoParallel(cl)



xgb_model <- train_(method = "xgbTree", tuneLength = 100)
write_model(xgb_model, "xgb")
write_submission_(xgb_model)

glmnet_model <- train_(method = "glmnet", preProcess = c("center", "scale"), tuneLength = 100)
write_model(glmnet_model, "glmnet")
write_submission_(glmnet_model)

rf_model <- train_(method = "rf", tuneLength = 11, ntree = 2000)
write_model(rf_model, "rf")
write_submission_(rf_model)







xgb_model <- train(X_train, y_train, method = "xgbTree", trControl = trControl, tuneLength = 100)

glmnet_model <- train(X_train, y_train, method = "glmnet", preProcess = c("center", "scale"), trControl = trControl, tuneLength = 100)

rf_model <- train(X_train, y_train, method = "rf", trControl = trControl, tuneLength = 100)

lm_model <- train(X_train, y_train, method = "lm", preProcess = c("center", "scale"), trControl = trControl, tuneLength = 10)

model_blend <- blending(list(xgb=xgb_model, glmnet=glmnet_model, rf=rf_model, lm=lm_model), X_train, y_train, X_test,
                        method="glmnet", tuneLength = 100)

write_models_log(model_blend, "blending_xgb_glmnet_rf_lm")

save_submission(data_frame(Id = df_test$Id,
                           SalePrice = exp(model_blend$y_pred)))

stopCluster(cl)


