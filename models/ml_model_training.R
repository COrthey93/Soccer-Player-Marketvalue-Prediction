# GENERAL LIBRARIES
set.seed(123)
library(dplyr)
library(ggplot2)

# ML FRAMEWORK LIBRARIES
library(caret)
library(e1071)
library(doParallel)
registerDoParallel(makePSOCKcluster(parallel::detectCores()-4))

# ML MODEL LIBRARIES
library(ranger)
library(glmnet)
library(neuralnet)

ml_data.norm <- readRDS("data/ml_data_norm.rds")
ml_data.denorm <- readRDS("data/ml_data_denorm.rds")

# 0.  DATA SPLITTING
train_Index <- createDataPartition(ml_data.norm$value_eur, p = .8,
                                   list = F,
                                   times = 1)
train       <- ml_data.norm[train_Index,]
test        <- ml_data.norm[-train_Index,]

# 1.  LINEAR MODEL
control_lm   <- trainControl(method = "cv", number = 5, savePredictions = "all")
lm.1         <- train(value_eur ~ ., data = train, method = "lm", trControl = control_lm)

# 2.  DECISION TREE MODELS
metric_rf    <- "RMSE"
control_rf   <- trainControl(method = "cv", number = 5, savePredictions = "all")
rf.1         <- caret::train(value_eur ~ ., data = train, method = "ranger", trControl = control_rf, metric = metric_rf)

# 3.  NEURAL NETWORK MODEL (MULTICORE CPU)
metric_nn    <- "RMSE"
control_nn   <- trainControl(method = "cv", number = 5, savePredictions = "all")
tune.grid.nn <- expand.grid(layer1 = 32, layer2 = 16, layer3 = 16)

nn.1         <- caret::train(value_eur ~ .,
                            data = train,
                            method = "neuralnet",
                            trControl = control_nn,
                            metric = metric_nn,
                            tuneGrid = tune.grid.nn,
                            linear.output = TRUE,
                            lifesign = "full",
                            threshold = 0.01
                            )

# 4.  MODEL SAVING
saveRDS(object = fs.1, file = "models/fs_model.rds")  # Saving the Feature Selection Model (glmnet)
saveRDS(object = lm.1, file = "models/lm_model.rds")  # Saving the Linear Model (lm)
saveRDS(object = rf.1, file = "models/rf_model.rds")  # Saving the Random Forest Model (ranger)
saveRDS(object = nn.1, file = "models/nn_model.rds")  # Saving the Neural Network Model (neuralnet)

# 5.  MODEL COMPARISON
results     = resamples(
  list(
    'Linear Model' = lm.1,
    'Random Forest' = rf.1,
    'Neural Network' = nn.1
  )
)
comp_tab <- summary(results)
comp_tab <- as.data.frame(comp_tab)
# -> stargazer latex table?

# 5.  TEST SET PREDICTION OF CHOSEN MODEL (-> ACCURACY CALCULATION)
prediction_df <- function(ml_model = rf.1){
  df = test
  df$prediction = predict(ml_model, df)
  df$deviation = df$value_eur - df$prediction
  df$deviation_sqrt = df$deviation^2
  return(df)
}

prediction_df_lm <- prediction_df(ml_model = lm.1)
prediction_df_rf <- prediction_df(ml_model = rf.1)
prediction_df_nn <- prediction_df(ml_model = nn.1)

rmse_lm <- sqrt(mean(prediction_df_lm$deviation_sqrt, na.rm = T))
rmse_rf <- sqrt(mean(prediction_df_rf$deviation_sqrt, na.rm = T))
rmse_nn <- sqrt(mean(prediction_df_nn$deviation_sqrt, na.rm = T))

print(c(rmse_lm, rmse_rf, rmse_nn))
rmse_df <- data.frame('Linear Model' = rmse_lm,
     'Random Forest' = rmse_rf,
     'Neural Network' = rmse_nn)
print(rsme_df)

# --> The Neural Network Model is the best Model regarding to the Root Mean Squared Error
