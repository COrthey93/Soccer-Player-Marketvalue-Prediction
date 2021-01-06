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

# 1.  DATASOURCING & CLEANING
players_21  <- read.csv("data/players_21.csv", stringsAsFactors=TRUE, encoding = "UTF-8")    # Reading the initial data 
players_21  <- players_21[players_21$player_positions != "GK",]                              # Deleting all Goalkeepers from the dataframe
players_21  <- players_21[,colSums(is.na(players_21))==0]                                    # Substituting all NAs by Zeros
players_21  <- players_21[players_21$value_eur != 0,]                                        # Deleting all 0 values from the market value
nums        <- unlist(lapply(players_21, is.numeric))                                        # Identifying all numeric columns
data        <- players_21[,nums]                                                             # Subsetting the data to only numeric columns
drop        <- c("overall", "sofifa_id")                                                     # Deleting Overall (consists of single abilities) and ID (as primary key unrelated to data
data        <- data[,!names(data) %in% drop] 

# 2.  PRE-PROCESSING


# 3.  DATA SPLITTING
train_Index <- createDataPartition(data$value_eur, p = .8,
                                   list = F,
                                   times = 1)
train       <- data[train_Index,]
test        <- data[-train_Index,]

# 4.  FEATURE SELECTION
control_fs  <- trainControl(method = "cv", number = 10)
fs.1        <- train(value_eur~., data=train, method="glmnet", trControl = control_fs)
importance  <- varImp(fs.1)
importance  <- data.frame(importance[1])
cols        <- rownames(importance)[order(importance$Overall, decreasing = T)[1:9]]
cols        <- append(cols, "value_eur")

ml_data     <- data[cols]

# 2. BOXPLOTS
for(i in 1:ncol(ml_data)) {
  boxplot(df[,i], main=names(df)[i])
}
