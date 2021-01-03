# GENERAL LIBRARIES
set.seed(123)
library(dplyr)
library(ggplot2)

# ML FRAMEWORK LIBRARIES
library(caret)
library(e1071)

# ML MODEL LIBRARIES
library(ranger)
library(glmnet)

# 1. DATASOURCING & CLEANING
players_21  <- read.csv("../data/players_21.csv", stringsAsFactors=TRUE, encoding = "UTF-8")    # Reading the initial data 
players_21  <- players_21[players_21$player_positions != "GK",]                              # Deleting all Goalkeepers from the dataframe
players_21  <- players_21[,colSums(is.na(players_21))==0]                                    # Substituting all NAs by Zeros
players_21  <- players_21[players_21$value_eur != 0,]                                        # Deleting all 0 values from the market value
nums        <- unlist(lapply(players_21, is.numeric))                                        # Identifying all numeric columns
tmp         <- players_21[,nums]                                                             # Subsetting the data to only numeric columns
drop        <- c("overall", "sofifa_id")                                                     # Deleting Overall (consists of single abilities) and ID (as primary key unrelated to data)
tmp         <- tmp[,!names(tmp) %in% drop]                                                   # "
pp_values   <- preProcess(tmp, method = c("center", "scale"))
tmp         <- predict(pp_values, tmp)

control_fs  <- trainControl(method = "cv", number = 10)
fs.1        <- train(value_eur~., data=tmp, method="glmnet", trControl = control_fs)
importance  <- varImp(fs.1)
importance  <- data.frame(importance[1])
cols        <- rownames(importance)[order(importance$Overall, decreasing = T)[1:9]]
cols        <- append(cols, "value_eur")

df          <- tmp[cols]

train_Index <- createDataPartition(df$value_eur, p = .8,
                                               list = F,
                                               times = 1)
train       <- df[train_Index,]
test        <- df[-train_Index,]

#apply(train,2,function(x) sum(is.na(x))) #-> no NA values present in the train dataset. Good!

# 2. BOXPLOTS
for(i in 1:ncol(df)) {
  boxplot(df[,i], main=names(df)[i])
}
