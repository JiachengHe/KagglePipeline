devtools::load_all()
library(readr)
library(dplyr)
library(ggplot2)
library(caret)
library(doParallel)

options(readr.num_columns = 0)
df_train <- read_csv("House_Prices/data/train.csv")
df_test <- read_csv("House_Prices/data/test.csv")

df_train$SalePrice <- log(df_train$SalePrice)
ylim <- c(min(df_train$SalePrice, na.rm = TRUE), max(df_train$SalePrice, na.rm = TRUE))

n_train <- nrow(df_train)
n_test <- nrow(df_test)

df <- bind_rows(mutate(df_train, train_or_test = "train"),
                mutate(df_test, train_or_test = "test"))

# factor_plot(df, "Neighborhood", "SalePrice", type="box", ylim = ylim) + coord_flip()
# factor_plot(df, "Neighborhood", "SalePrice", type="count", ylim = ylim) + coord_flip()
# automate_factor_plot(df, "SalePrice", ylim = ylim)

source("House_Prices/House_Prices_FE.R")

df <- automate_factor_to_lmfit(df, "SalePrice")

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




nzv <- nearZeroVar(df, freqCut = 95/5, saveMetrics = TRUE)
rownames(nzv[nzv$nzv,])
sapply(rownames(nzv[nzv$nzv,]), function(x){table(df[[x]])})
sapply(rownames(nzv[nzv$nzv,]), function(x){is.numeric(df[[x]])})


df_train <- df %>%
  filter(train_or_test == "train") %>%
  select(-Id, -train_or_test)

df_test <- df %>%
  filter(train_or_test == "test") %>%
  select(-Id, -train_or_test, -SalePrice)

X_train <- select(df_train, -SalePrice) %>% as.matrix()
y_train <- df_train$SalePrice
X_test <- as.matrix(df_test)

cl <- makeCluster(detectCores(logical = FALSE) - 1, type = "SOCK")
registerDoParallel(cl)

glmnet_control <- trainControl(method = "cv", number = 5, returnData = FALSE, savePredictions = "final", search = "random")
glmnet_model <- train(X_train, y_train, method = "glmnet", preProcess = c("center", "scale"),
                      trControl = glmnet_control, tuneLength = 100)
glmnet_model$bestTune
glmnet_model$results[10,]

xgb_control <- trainControl(method = "cv", number = 5, returnData = FALSE, savePredictions = "final", search = "random")
xgb_model <- train(X_train, y_train, method = "xgbTree", nrounds = 2000, trControl = xgb_control, tuneLength = 2)
xgb_model$results

stopCluster(cl)


submission <- data_frame(Id = Id,
                         SalePrice = exp(predict(glmnet_model, X_test)))
write.csv(submission, file = "House_Prices/submission_6.csv", row.names = FALSE)


submission <- data_frame(Id = Id,
                         SalePrice = exp(predict(xgb_model, X_test)))
write.csv(submission, file = "House_Prices/submission_4.csv", row.names = FALSE)



lm_model <- lm(SalePrice ~ ., data = df_train)
Id <- df_test$Id
submission <- data_frame(Id = Id,
                         SalePrice = exp(predict(lm_model, newdata = df_test)))
write.csv(submission, file = "House_Prices/submission_1.csv", row.names = FALSE)





models_blend <- blending(list(glmnet=glmnet_model, xgb=xgb_model), X_train, y_train, X_test, method="xgbTree")
