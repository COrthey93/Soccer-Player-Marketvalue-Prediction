### 1. LOAD LIBRARIES
# install.packages("caret")
# install.packages("randomForest")
# install.packages("e1071")
# install.packages("doParallel")

library(caret)
library(randomForest)
library(e1071)
library(doParallel)
set.seed(80085)
registerDoParallel(makePSOCKcluster(2))

###   2. LOAD & PREPARE DATA
players_21  <- read.csv("data/players_21.csv", stringsAsFactors=TRUE, encoding = "UTF-8")
##    2.1 Subset numeric columns
nums        <- unlist(lapply(players_21, is.numeric))  
tmp         <- players_21[,nums]
##    2.2 Defining columns to use for further analysis
cols        <- c("value_eur", 
                 "age", "potential", "international_reputation", 
                 "overall", "skill_moves", "pace", "shooting", "passing", "dribbling", "defending", "physic")
df          <- tmp[cols]
df          <- na.omit(df)

###   3. DESCRIPTIVE STATISTICS

###   4. PLOTS
##    4.1 Correlation Plots
plot(df$age, df$value_eur)

##    4.2 Boxplots
for(i in 1:ncol(df)) {
  boxplot(df[,i], main=names(df)[i])
}

###   5. TRAINING ML MODEL
##    5.1 Specification of Metrics
control     = trainControl(method = "cv", number = 10)
metric      = "Rsquared"

##    5.2 Train/Test Split
train_Index = createDataPartition(df$international_reputation, p = .8,
                                  list = F,
                                  times = 1)
train       = df[train_Index,]
test        = df[-train_Index,]

##    5.3 Regression Models (value_eur on overall + covariates)
##        How does the overall skill level influence the value of the players?

#     5.3.1 Linear Model 
lm.1        = train(value_eur ~ ., data = train, method = "lm", trControl = control, metric = metric)
summary(lm.1)

lm.1        = lm(value_eur ~ ., data = df)
summary(lm.1)

#     5.3.2 Generalized Linear Model
glm.1       = glm(value_eur ~ ., data = df)
summary(glm.1)

##    5.4 Decision Tree Model
rf.1        = train(value_eur ~ ., data = train, method = "rf", trControl = control, metric = metric)
summary(rf.1)
#     5.4.1 Feature Importance

###   6. MEASUREMENTS
results     = resamples(list(lm = lm.1, rf = rf.1))
summary(results)

###   7. PREDICTION

