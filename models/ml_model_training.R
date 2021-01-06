# GENERAL LIBRARIES
set.seed(123)
library(dplyr)
library(ggplot2)

# ML FRAMEWORK LIBRARIES
library(caret)
library(e1071)
library(doParallel)
registerDoParallel(makePSOCKcluster(parallel::detectCores()-2))

# ML MODEL LIBRARIES
library(ranger)
library(glmnet)
library(neuralnet)
library(kernlab)

ml_data.norm <- readRDS("data/ml_data_norm.rds")
ml_data.denorm <- readRDS("data/ml_data_denorm.rds")

# 0.  DATA SPLITTING
train_Index <- createDataPartition(ml_data.norm$value_eur, p = .8,
                                   list = F,
                                   times = 1)
train       <- ml_data.norm[train_Index,]
test        <- ml_data.norm[-train_Index,]

# 1.  LINEAR MODEL
control_lm   <- trainControl(method = "cv", number = 10, savePredictions = "all")
lm.1         <- train(value_eur ~ ., data = train, method = "lm", trControl = control_lm)

# 2.  SUPPORT VECTOR MACHINE
metric_svm   <- "RMSE"
control_svm  <- trainControl(method = "cv", number = 10, savePredictions = "all")
svm.1        <- train(value_eur ~ ., data = train, method = "svmLinear", trControl = control_svm, metric = metric_svm)

# 3.  DECISION TREE MODELS
metric_rf    <- "RMSE"
control_rf   <- trainControl(method = "cv", number = 10, savePredictions = "all")
rf.1         <- train(value_eur ~ ., data = train, method = "ranger", trControl = control_rf, metric = metric_rf)

# 4.  NEURAL NETWORK MODEL (MULTICORE CPU)
metric_nn    <- "RMSE"
control_nn   <- trainControl(method = "cv", number = 10, savePredictions = "all")
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

# 5.  MODEL SAVING
saveRDS(object = fs.1, file = "models/fs_model.rds")  # Saving the Feature Selection Model (glmnet)
saveRDS(object = lm.1, file = "models/lm_model.rds")  # Saving the Linear Model (lm)
saveRDS(object = svm.1, file = "models/svm_model.rds")# Saving the Support Vector Machine Model (kernlab)
saveRDS(object = rf.1, file = "models/rf_model.rds")  # Saving the Random Forest Model (ranger)
saveRDS(object = nn.1, file = "models/nn_model.rds")  # Saving the Neural Network Model (neuralnet)

# 6.  MODEL COMPARISON
results     = resamples(
  list(
    'Linear Model' = lm.1,
    'Support Vector Machine' = svm.1,
    'Random Forest' = rf.1,
    'Neural Network' = nn.1
  )
)
summary(results)

# 7.  TEST SET PREDICTION OF CHOSEN MODEL (-> ACCURACY CALCULATION)
prediction_df <- function(ml_model = rf.1){
  df = test
  df$prediction = predict(ml_model, df)
  df$abs_deviation = abs(df$value_eur - df$prediction)
  return(df)
}

prediction_df_lm <- prediction_df(ml_model = lm.1)
prediction_df_rf <- prediction_df(ml_model = rf.1)
prediction_df_nn <- prediction_df(ml_model = nn.1)

mean(prediction_df_lm$abs_deviation)
mean(prediction_df_rf$abs_deviation)
mean(prediction_df_nn$abs_deviation)

# --> The Random Forest Model is the best Model regarding to the Mean Absolute Deviation
