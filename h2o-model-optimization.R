# Load packages and data
setwd("~/Dropbox/Courses/Kaggle/Two-sigma-Rental")
# Loading libraries
library(h2o)
library(ggplot2)
# h2o.init(nthreads = -1)
h2o.init(nthreads = -1, max_mem_size = "16g")

# load(file = "RData/TrainProcessed.RData")
load(file = "RData/AllData.RData")

AllData$listing_id <- as.numeric(as.character(AllData$listing_id))
AllData$building_id <- as.factor(AllData$building_id)
for (i in 22:66) {
  AllData[,i] <- as.numeric(as.character(AllData[,i]))
}

# AllData <- AllData[,-(95:99)]
# AllData <- AllData[,-(68:93)]
# AllData <- AllData[,-(13:17)]

t1 <- AllData[1:49352,]
t2 <- t1[,-4] # Remove created
# t2 <- t2[,-1]  # Remove listing_id
# t2 <- t2[,-10]  # Remove manager_id
# t2 <- t2[,-91]  # Remove building_id
# t2 <- t2[,-(19:63)]  # Remove some engineered features
# t2 <- t2[,-(91:95)]  # Remove all building features
# t2 <- t2[,-(10:14)]  # Remove all manager features

train <- as.h2o(t2, destination_frame = "train.hex")
varnames <- setdiff(colnames(train), "interest")

# ntreesVar = seq(100, 2100, 300)
# results.ntreesVar <- data.frame(ntrees = numeric(0),
#                             trainLoss = numeric(0), stringsAsFactors=FALSE)

maxdepth = seq(3, 15, 3)
results.maxdepth <- data.frame(maxdepth = numeric(0),
                                trainLoss = numeric(0),
                                testLoss = numeric(0), stringsAsFactors=FALSE)

# minrows = seq(100, 200, 5)
# results.minrows <- data.frame(minrows = numeric(0), 
#                                trainLoss = numeric(0), 
#                                testLoss = numeric(0), stringsAsFactors=FALSE)

min <- 0.51
for (i in 1:length(maxdepth)) {
  results.maxdepth[i,1] <- maxdepth[i]
  
  gbm1 <- h2o.gbm(x = varnames,
                  y = "interest_level",
                  training_frame = train,
                  distribution = "multinomial",
                  model_id = "gbm1",
                  nfolds = 3,
                  #ntrees = ntreesVar[i],
                  ntrees = 2500,
                  learn_rate = 0.1,
                  #max_depth = maxdepth[i],
                  max_depth = 7,
                  #min_rows = minrows[i],
                  min_rows = 125,
                  sample_rate = 0.8,
                  col_sample_rate = 0.7,
                  stopping_rounds = 5,                  
                  stopping_metric = "logloss",
                  stopping_tolerance = 0.002,
                  seed=321
  )
  
  results.maxdepth[i,2] <- h2o.logloss(gbm1)
  results.maxdepth[i,3] <- h2o.logloss(gbm1, xval = TRUE)
  
  print(i)
  print(h2o.logloss(gbm1))
  print(h2o.logloss(gbm1, xval = TRUE))
  
  if(h2o.logloss(gbm1, xval = TRUE) < min) {
    min <-  h2o.logloss(gbm1, xval = TRUE)
    h2o.saveModel(gbm1, path= "RData/", force = TRUE)
    print("Model saved")
  } 
   
}

# gbm1_noManager_Building <- gbm1
# h2o.saveModel(gbm1_noManager_Building, path= "RData/")

# gbm1_Manager_noBuilding <- gbm1
# h2o.saveModel(gbm1_Manager_noBuilding, path= "RData/")

# gbm1_noManager_noBuilding <- gbm1
# h2o.saveModel(gbm1_noManager_noBuilding, path= "RData/")

# gbm1_Manager_Building <- gbm1
# h2o.saveModel(gbm1_Manager_Building, path= "RData/")

h2o.saveModel(gbm1, path= "RData/", force = TRUE)

# save(results.ntreesVar, file = "RData/resultsntreesVar.RData")
# ggplot(data=results.ntreesVar, aes(x = ntrees, y = value)) + geom_line(aes(y = trainLoss, colour = "Train"))  + geom_line(aes(y = testLoss, colour = "Loss"))
# 
# save(results.maxdepth, file = "RData/resultsmaxdepth.RData")
# ggplot(data=results.maxdepth, aes(x = maxdepth, y = value)) + geom_line(aes(y = trainLoss, colour = "Train"))  + geom_line(aes(y = testLoss, colour = "Loss"))
# 
# save(results.minrowsFine, file = "RData/resultsminrows.RData")
# ggplot(data=results.minrowsFine, aes(x = minrows, y = value)) + geom_line(aes(y = trainLoss, colour = "Train"))  + geom_line(aes(y = testLoss, colour = "Loss"))

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

