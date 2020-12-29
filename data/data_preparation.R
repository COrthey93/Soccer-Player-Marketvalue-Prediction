library(caret)

# 1. DATASOURCING & CLEANING
players_21  <- read.csv("../data/players_21.csv", stringsAsFactors=TRUE, encoding = "UTF-8")
nums        <- unlist(lapply(players_21, is.numeric))  
tmp         <- players_21[,nums]
cols        <- c("value_eur", 
                 "age", "potential", "international_reputation", 
                 "overall", "skill_moves", "pace", "shooting", "passing", "dribbling", "defending", "physic")
df          <- tmp[cols]
df          <- na.omit(df)

# 2. BOXPLOTS
for(i in 1:ncol(df)) {
  boxplot(df[,i], main=names(df)[i])
}

# 3. TRAIN & TEST SPLIT
train_Index = createDataPartition(df$international_reputation, p = .8,
                                  list = F,
                                  times = 1)
train       = df[train_Index,]
test        = df[-train_Index,]

# 4. CONTROL METRIC DEFINITION
control     = trainControl(method = "cv", number = 10)
metric_lm   = "Rsquared"
metric_rf   = "RMSE"
