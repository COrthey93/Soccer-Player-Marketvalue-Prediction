# GENERAL LIBRARIES
set.seed(123)
library(dplyr)
library(ggplot2)

# ML FRAMEWORK LIBRARIES
library(caret)
library(e1071)
# library(doParallel)
# registerDoParallel(makePSOCKcluster(10))

# ML MODEL LIBRARIES
library(ranger)
library(glmnet)
library(neuralnet)
library(keras)

# 1.  DECISION TREE MODELS
metric_rf    <- "RMSE"
control_rf   <- trainControl(method = "cv", number = 10, savePredictions = "all")
rf.1         <- train(value_eur ~ ., data = train, method = "ranger", trControl = control, metric = metric_rf)

# 2.  NEURAL NETWORK MODEL (KERAS GPU)


# 2.  NEURAL NETWORK MODEL (MULTICORE CPU)
metric_nn    <- "RMSE"
control_nn   <- trainControl(method = "cv", number = 2, savePredictions = "all")
tune.grid.nn <- expand.grid(layer1 = 512, layer2 = 256, layer3 = 256)

nn.1         <- caret::train(value_eur ~ .,
                            data = train,
                            method = "neuralnet",
                            trControl = control_nn,
                            metric = metric_nn,
                            tuneGrid = tune.grid.nn,
                            linear.output = TRUE,
                            lifesign = "full",
                            threshold = 0.5)

# 3  MODEL SAVING
# saveRDS(object = fs.1, file = "models/fs_model.rds")  # Saving the Feature Selection Model (glmnet)
# saveRDS(object = rf.1, file = "models/rf_model.rds")  # Saving the Random Forest Model (ranger)
# saveRDS(object = nn.1, file = "models/nn_model.rds") # Saving the Neural Network Model (neuralnet)

# 4.  MODEL COMPARISON
results     = resamples(
  list(
    'Random Forest' = rf.1,
    'Neural Network' = nn.1
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

hist(result$percent_deviation)