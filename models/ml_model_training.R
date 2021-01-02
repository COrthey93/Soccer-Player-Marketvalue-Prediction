# ML MODEL LIBRARIES
library(caret)
library(randomForest)
library(e1071)
library(doParallel)
library(ranger)
library(ggplot2)
library(neuralnet)
#set.seed(80085)
registerDoParallel(makePSOCKcluster(8)) # CAUTION with this line! Number of cores used may not be sufficient for your PC!

# 0.  IDENTIFY VARIABLES USED FOR MODEL BUILDING


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
normalize <- function(x) {
  return ((x - min(x)) / (max(x)- min(x)))
}


train_Index = createDataPartition(df$international_reputation, p = .05,
                                  list = F,
                                  times = 1)
traintest       = df[train_Index,]
test        = df[-train_Index,]

traintestn$value_eur <- traintest$value_eur
traintestn$age <- normalize(traintest$age)


tune.grid.neuralnet <- expand.grid(
  layer1 = 256,
  layer2 = 128,
  layer3 = 128,
  layer4 = 64
)

nn.1 <- caret::train(value_eur ~ age + potential + international_reputation + overall + skill_moves + pace + shooting + passing + dribbling + defending + physic, 
                     data = traintest, 
                     method = "neuralnet", 
                     trControl = control , 
                     metric = metric_rf, 
                     stepmax = 1e6, rep = 1, 
                     linear.output = TRUE,
                     #algorithm = "backprop",
                     #learningrate = 0.01,
                     lifesign = "full", 
                     threshold = 0.1  )

getModelInfo("neuralnet")$neuralnet$parameters
nn.1$results
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
