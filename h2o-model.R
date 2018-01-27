# Load packages and data
setwd("~/Dropbox/Courses/Kaggle/Two-sigma-Rental")
# Loading libraries
library(jsonlite)
library(dplyr)
library(purrr)
library(knitr)
library(stringr)
library(data.table)
library(h2o)
library(lubridate)
# h2o.init(nthreads = -1, max_mem_size = "10g")
h2o.init(max_mem_size = "16g")

# Loading train data
train <- fromJSON("Data/train.json")
# Set all variable names except `photos`` and `feactures`` into an array
column <- setdiff(names(train), c("photos", "features"))
# Unlist every variable except `photos` and `features` and convert to tibble
train <- map_at(train, column, unlist) %>% tibble::as_tibble(.)

# Loading fillDataFrame function to create data frame after feature engineering
source("FillDataFrame.R")  

# Process train data with feature engineering
# load(file = "RData/TrainProcessed.RData")
t1 <- fillDataFrame(train)
save(t1, file = "RData/TrainProcessed.RData")

t1 <- AllData[1:49352,]
t2 <- t1[,-4] # Remove created
#t2 <- t2[,-(21:65)]  # Remove some engineered features
#t2 <- t2[,-(22:31)]  # Remove some engineered features
t2 <- t2[,-1]  # Remove listing_id
t2 <- t2[,-10]  # Remove manager_id
t2 <- t2[,-91]  # Remove building_id
t2 <- t2[,-(91:95)]  # Remove all building features
t2 <- t2[,-(10:14)]  # Remove all manager features

# Use this just for testing
random <- sample(nrow(t2), 35000)
t2 <- t2[random,]

train <- as.h2o(t2, destination_frame = "train.hex")

varnames <- setdiff(colnames(train), "interest")
gbm1 <- h2o.gbm(x = varnames,
                y = "interest",
                training_frame = train,
                distribution = "multinomial",
                model_id = "gbm1",
                nfolds = 5,
                ntrees = 1200,
                learn_rate = 0.01,
                max_depth = 7,
                min_rows = 125,
                sample_rate = 0.8,
                col_sample_rate = 0.7,
                stopping_rounds = 5,
                stopping_metric = "logloss",
                stopping_tolerance = 0,
                seed=321
)

# Predictions on the train data
predsTrain <- as.data.table(h2o.predict(gbm1, train))
trainPreds <- data.table(interest = unlist(t2$interest), predsTrain[,.(high, medium, low)])
MultiLogLoss(trainPreds$interest, trainPreds$high, trainPreds$medium, trainPreds$low)

# Loading test data
test <- fromJSON("Data/test.json")
# Set all variable names except `photos`` and `feactures`` into an array
column <- setdiff(names(test), c("photos", "features"))
# Unlist every variable except `photos` and `features` and convert to tibble
test <- map_at(test, column, unlist) %>% tibble::as_tibble(.)

# Process train data with feature engineering
# load(file = "RData/TestProcessed.RData")
s1 <- fillDataFrame(test)
save(s1, file = "RData/TestProcessed.RData")

s1 <- AllData[49353:124011,]
s2 <- s1[,-4] # Remove created
#s2 <- s2[,-(21:65)]  # Remove some engineered features
#s2 <- s2[,-(22:31)]  # Remove some engineered features
s2 <- s2[,-1]  # Remove listing_id
s2 <- s2[,-10]  # Remove manager_id
s2 <- s2[,-91]  # Remove building_id

# Use this just for testing
t2 <- t1[,-4] # Remove created
#t2 <- t2[,-(21:65)]  # Remove some engineered features
#t2 <- t2[,-(22:31)]  # Remove some engineered features
t2 <- t2[,-1]  # Remove listing_id
t2 <- t2[,-10]  # Remove manager_id
t2 <- t2[,-91]  # Remove building_id
s2 <- t2[!(rownames(t2) %in% random),]

test <- as.h2o(s2[,-18], destination_frame = "test.hex")

# Use this just for testing
preds <- as.data.table(h2o.predict(gbm1_noEngFeatures, test))
testPreds <- data.table(interest = unlist(s2$interest), preds[,.(high, medium, low)])
MultiLogLoss(testPreds$interest, testPreds$high, testPreds$medium, testPreds$low)

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# Predictions on the test data (single model) -----------------------

# load(file = "RData/TrainProcessed.RData")
load(file = "RData/AllData.RData")

AllData$listing_id <- as.numeric(as.character(AllData$listing_id))
AllData$building_id <- as.factor(AllData$building_id)
for (i in 22:66) {
  AllData[,i] <- as.numeric(as.character(AllData[,i]))
}

s1 <- AllData[49353:124011,]
s2 <- s1

test <- as.h2o(s2[,-21], destination_frame = "test.hex")
# test <- as.h2o(s2[,-16], destination_frame = "test.hex")

# Loading model
gbm1 <- h2o.loadModel(path= "RData/test")

preds <- as.data.table(h2o.predict(gbm1, test))
testPreds <- data.table(listing_id = unlist(s1$listing_id), preds[,.(high, medium, low)])
fwrite(testPreds, "submission.csv")

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# Predictions on the test data (mixing models) ----------------------

s1 <- AllData[49353:124011,]
s2 <- s1

gbm1_Manager_Building <- h2o.loadModel("RData/gbm1_Manager_Building")
preds <- as.data.table(
  h2o.predict(gbm1_Manager_Building, as.h2o(s2[s2$managerNoLis != 0 & s2$buildingNoLis != 0 ,][,-21], destination_frame = "test.hex")) )
testPreds <- data.table(listing_id = unlist(s2[s2$managerNoLis != 0 & s2$buildingNoLis != 0 ,]$listing_id), 
                                           preds[,.(high, medium, low)])
testPredFinal <- testPreds

gbm1_noManager_Building <- h2o.loadModel("RData/gbm1_noManager_Building")
preds <- as.data.table(
  h2o.predict(gbm1_noManager_Building, as.h2o(s2[s2$managerNoLis == 0 & s2$buildingNoLis != 0 ,][,-21], destination_frame = "test.hex")) )
testPreds <- data.table(listing_id = unlist(s2[s2$managerNoLis == 0 & s2$buildingNoLis != 0 ,]$listing_id), 
                                           preds[,.(high, medium, low)])
testPredFinal <- rbind(testPredFinal, testPreds)

gbm1_Manager_noBuilding <- h2o.loadModel("RData/gbm1_Manager_noBuilding")
preds <- as.data.table(
  h2o.predict(gbm1_Manager_noBuilding, as.h2o(s2[s2$managerNoLis != 0 & s2$buildingNoLis == 0 ,][,-21], destination_frame = "test.hex")) )
testPreds <- data.table(listing_id = unlist(s2[s2$managerNoLis != 0 & s2$buildingNoLis == 0 ,]$listing_id), 
                        preds[,.(high, medium, low)])
testPredFinal <- rbind(testPredFinal, testPreds)

gbm1_noManager_noBuilding <- h2o.loadModel("RData/gbm1_noManager_noBuilding")
preds <- as.data.table(
  h2o.predict(gbm1_noManager_noBuilding, as.h2o(s2[s2$managerNoLis == 0 & s2$buildingNoLis == 0 ,][,-21], destination_frame = "test.hex")) )
testPreds <- data.table(listing_id = unlist(s2[s2$managerNoLis == 0 & s2$buildingNoLis == 0 ,]$listing_id), 
                        preds[,.(high, medium, low)])
testPredFinal <- rbind(testPredFinal, testPreds)

fwrite(testPredFinal, "submission.csv")


# ----------------------------------------
MultiLogLoss <- function(act, predH, predM, predL)
{
  eps = 1e-15;
  n <- length(act)
  logloss <- -1/n*sum(sapply(1:n, function(i) ifelse( act[i]=="high", log(max(min(predH[i],1-eps),eps)), 
                                                      ifelse( act[i]=="medium", log(max(min(predM[i],1-eps),eps)), 
                                                              log(max(min(predL[i],1-eps),eps)) ) ) ) )
  return(logloss);
}

