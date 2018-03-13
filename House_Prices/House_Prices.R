devtools::load_all()
library(readr)
library(dplyr)
library(ggplot2)
library(caret)
library(doParallel)
if (Sys.info()['sysname'] == "Linux") { library(Rmpi) }
if ("House_Prices" %in% list.dirs(recursive = FALSE, full.names = FALSE)) { setwd("House_Prices") }

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


if (Sys.info()['sysname'] == "Windows") {
  num_cores <- detectCores(logical = FALSE) - 1
  cl <- makeCluster(num_cores, type = "SOCK")
} else if (Sys.info()['sysname'] == "Linux") {
  num_nodes <- mpi.universe.size() - 1
  cl <- makeCluster(num_nodes, type = "MPI")
}

registerDoParallel(cl)



xgbTree_model <- train_(method = "xgbTree", tuneLength = 1000)
write_model(xgbTree_model, "xgbTree")
write_submission_(xgbTree_model)

glmnet_model <- train_(method = "glmnet", preProcess = c("center", "scale"), tuneLength = 500)
write_model(glmnet_model, "glmnet")
write_submission_(glmnet_model)

rf_model <- train_(method = "rf", tuneLength = 500, ntree = 2000)
write_model(rf_model, "rf")
write_submission_(rf_model)

xgbLinear_model <- train_(method = "xgbLinear", preProcess = c("center", "scale"), tuneLength = 1000)
write_model(xgbLinear_model, "xgbLinear")
write_submission_(xgbLinear_model)



model_list <- list(xgbTree=xgbTree_model, glmnet=glmnet_model, rf=rf_model, xgbLinear=xgbLinear_model)
blending_ <- purrr::partial(blending, model_list = model_list, X_train = X_train, y_train = y_train,
                            X_test = X_test, trControl = trCon)


blend_glmnet <- blending_(method="glmnet", tuneLength = 200)
write_model(blend_glmnet$blend_model, "blend_glmnet")
write_submission(data_frame(Id = df_test$Id,
                            SalePrice = exp(blend_glmnet$y_pred)))


blend_xgbL <- blending_(method="xgbLinear", tuneLength = 1000)
write_model(blend_xgbL$blend_model, "blend_xgbL")
write_submission(data_frame(Id = df_test$Id,
                            SalePrice = exp(blend_xgbL$y_pred)))

stopCluster(cl)


