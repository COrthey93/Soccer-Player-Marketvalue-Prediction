# ML MODEL LIBRARIES
library(caret)
library(randomForest)
library(e1071)
library(doParallel)
library(ranger)
library(ggplot2)
set.seed(80085)
#registerDoParallel(makePSOCKcluster(8)) # CAUTION with this line! Number of cores used may not be sufficient for your PC!

# 1.  LINEAR REGRESSION MODELS
lm.1        = train(value_eur ~ ., data = train, method = "lm", trControl = control, metric = metric_lm)
summary(lm.1)

lm.1        = lm(value_eur ~ ., data = df)
summary(lm.1)

glm.1       = glm(value_eur ~ ., data = df)
summary(glm.1)

# 2.  DECIION TREE MODELS
rf.1        = train(value_eur ~ ., data = train, method = "ranger", trControl = control, metric = metric_rf)
summary(rf.1)

# 3.  LASSO/RIDGE REGRESSION MODEL

# 4.  NEURAL NETWORK MODEL

# 5.  MODEL SAVING
saveRDS(object = rf.1, file = "models/rf_model.rds")

# 6.  MODEL COMPARISON
results     = resamples(list('Linear Regression' = lm.1, 
                             'Random Forest' = rf.1))
summary(results)


############################################################################

###   8. PREDICTIONS
##    8.1 Prediction on Test Dataframe and calculating deviations
predictions = predict(rf.1, test)

result <- test[,2:ncol(test)]
result['value_eur'] <- test$value_eur
result['prediction'] <- predictions
result['deviation'] <- result$value_eur - result$prediction
result['percent_deviation'] <- (result$deviation / result$value_eur)*100

hist(result$percent_deviation)
boxplot(result$percent_deviation)
