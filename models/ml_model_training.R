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

# 1.  LINEAR MODEL
control_lm   <- trainControl(method = "cv", number = 5, savePredictions = "all")
lm.1         <- train(value_eur ~ ., data = train, method = "lm", trControl = control_lm, preProcess = c("center", "scale"))

# 2.  DECISION TREE MODELS
metric_rf    <- "RMSE"
control_rf   <- trainControl(method = "cv", number = 5, savePredictions = "all")
rf.1         <- train(value_eur ~ ., data = train, method = "ranger", trControl = control_rf, metric = metric_rf)

# 3.  NEURAL NETWORK MODEL (MULTICORE CPU)
metric_nn    <- "RMSE"
control_nn   <- trainControl(method = "cv", number = 5, savePredictions = "all")
tune.grid.nn <- expand.grid(layer1 = 512, layer2 = 256, layer3 = 256)

nn.1         <- caret::train(value_eur ~ .,
                            data = train,
                            method = "neuralnet",
                            trControl = control_nn,
                            metric = metric_nn,
                            tuneGrid = tune.grid.nn,
                            linear.output = TRUE,
                            lifesign = "full",
                            threshold = 0.5,
                            preProcess = c("center", "scale"))

# 4.  MODEL SAVING
# saveRDS(object = fs.1, file = "models/fs_model.rds")  # Saving the Feature Selection Model (glmnet)
# saveRDS(object = lm.1, file = "models/lm_model.rds")  # Saving the Linear Model (lm)
# saveRDS(object = rf.1, file = "models/rf_model.rds")  # Saving the Random Forest Model (ranger)
# saveRDS(object = nn.1, file = "models/nn_model.rds")  # Saving the Neural Network Model (neuralnet)

# 5.  MODEL COMPARISON
results     = resamples(
  list(
    'Linear Model' = lm.1,
    'Random Forest' = rf.1
  )
)
summary(results)

# 5.  TEST SET PREDICTION OF CHOSEN MODEL (-> ACCURACY CALCULATION)
predictions = predict(rf.1, test)

result                      <- test[,1:ncol(test)-1]
result['value_eur']         <- test$value_eur
result['prediction']        <- predictions
result['deviation']         <- result$value_eur - result$prediction
result['percent_deviation'] <- (result$deviation / result$value_eur)*100

print(mean(result$deviation))
