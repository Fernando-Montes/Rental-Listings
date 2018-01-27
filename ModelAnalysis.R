# Load packages and data
setwd("~/Dropbox/Courses/Kaggle/Two-sigma-Rental")
# Loading libraries
library(caret)
# Parallel computing
library(doParallel)
registerDoParallel(cores=2)

# Using new features identified and created in RelevantFeaturesIdentification.R
load(file = "RData/DataFeaturesFilled.RData")

# Using some of the data for testing
inTrain <- createDataPartition(data.features$interest, list = FALSE, p = 0.7)
my_train <- data.features[inTrain,]
my_test <- data.features[-inTrain,]

model_gbm <- prepare.model(my_train, "gbm")          # Model gbm 
save(model_gbm, file = "RData/model_gbm.rda")
load("RData/model_gbm.rda")
my_test$gbm_pred <- predict(model_gbm, my_test, type = "prob")

model_ranger <- prepare.model(my_train, "ranger")    # Model ranger
save(model_ranger, file = "RData/model_ranger.rda")
load("RData/model_ranger.rda")
my_test$ranger_pred <- predict(model_ranger, my_test, type = "prob")

model_glmnet <- prepare.model(my_train, "glmnet")    # Model glmnet
save(model_glmnet, file = "RData/model_glmnet.rda")
load("RData/model_glmnet.rda")
my_test$glmnet_pred <- predict(model_glmnet, my_test, type = "prob")

# Function to train method. Current choices: ranger, gbm, glmnet ------------
prepare.model <- function(my_train, methodchosen) {
  
  ptime <- system.time({
    if (methodchosen == "ranger") {  
      my_model <- train(
        interest ~ bathrooms + bedrooms + latitude + longitude + price + elevator + wood + 
          pets + noPets + doorman + dishwasher + laundryUnit + laundryBuilding + prewar + roofdeck +
          outdoorPublic + outdoorPrivate + diningroom + internet + balcony + pool + noFee + fitness +
          newConstruction + exclusive + highCeilings + renovated + park + livein + lounge + closet + playroom +
          ac + wheelchair + fireplace + loft + simplex + lowrise + highrise + furnished + duplex + green +
          storage + ss + light + misc + subway + shortTerm + view + postwar, 
        # method ="ranger", data = my_train, tuneGrid = expand.grid(mtry = c(3,4,6,10)), importance = 'impurity', metric="logLoss",
        method ="ranger", data = my_train, tuneGrid = expand.grid(mtry = c(10,12,14,16)), importance = 'impurity', metric="logLoss", 
        trControl = trainControl(method = "cv", number = 10, repeats = 50, 
                                 classProbs = TRUE, summaryFunction = mnLogLoss,
                                 verboseIter = TRUE, allowParallel = TRUE))
    } 
    else if (methodchosen == "gbm") {
      #gbmGrid <- expand.grid(.interaction.depth = (1:5) * 2, .n.trees = (1:10)*20, .shrinkage = .1, .n.minobsinnode = (5:15) )
      #gbmGrid <- expand.grid(.interaction.depth = c(6,8,10,12), .n.trees = c(200,500,1000,1500,2000,2500), .shrinkage = .1, .n.minobsinnode = (6:10) )
      gbmGrid <- expand.grid(.interaction.depth = 7, .n.trees = 2000, .shrinkage = .1, .n.minobsinnode = 20 )
      # my_model <- train( interest ~ bathrooms + bedrooms + latitude + longitude + price + elevator + wood + 
      #                      pets + noPets + doorman + dishwasher + laundryUnit + laundryBuilding + prewar + roofdeck +
      #                      outdoorPublic + outdoorPrivate + diningroom + internet + balcony + pool + noFee + fitness +
      #                      newConstruction + exclusive + highCeilings + renovated + park + livein + lounge + closet + playroom +
      #                      ac + wheelchair + fireplace + loft + simplex + lowrise + highrise + furnished + duplex + green +
      #                      storage + ss + light + misc + subway + shortTerm + view + postwar, 
      #                    method ="gbm", data = my_train, bag.fraction = 0.5, tuneGrid = gbmGrid, metric="logLoss",
      #                    trControl = trainControl(method = "cv", number = 10, repeats = 50,  
      #                                             classProbs = TRUE, summaryFunction = mnLogLoss,
      #                                             verboseIter = TRUE, allowParallel = TRUE))
      my_model <- train( interest ~ bathrooms + bedrooms + latitude + longitude + price + nphotos + ndescription + 
                           manager_id + yday + month+ mday + wday + hour +
                           price, 
                         method ="gbm", data = my_train, bag.fraction = 0.5, tuneGrid = gbmGrid, metric="logLoss",
                         trControl = trainControl(method = "cv", number = 10, repeats = 10,  
                                                  classProbs = TRUE, summaryFunction = mnLogLoss,
                                                  verboseIter = TRUE, allowParallel = TRUE))
    }
    else if (methodchosen == "glmnet") {
      my_model <- train( interest ~ bathrooms + bedrooms + latitude + longitude + price + elevator + wood + 
                           pets + noPets + doorman + dishwasher + laundryUnit + laundryBuilding + prewar + roofdeck +
                           outdoorPublic + outdoorPrivate + diningroom + internet + balcony + pool + noFee + fitness +
                           newConstruction + exclusive + highCeilings + renovated + park + livein + lounge + closet + playroom +
                           ac + wheelchair + fireplace + loft + simplex + lowrise + highrise + furnished + duplex + green +
                           storage + ss + light + misc + subway + shortTerm + view + postwar, 
                         method ="glmnet", data = my_train, tuneGrid = expand.grid(alpha = seq(0, 1, length = 10), 
                                                                                   lambda = seq(0.0001, 1, length = 20)), 
                         preProcess = c("center", "scale"), metric="logLoss",
                         trControl = trainControl(method = "cv", number = 10, repeats = 50, 
                                                  classProbs = TRUE, summaryFunction = mnLogLoss,
                                                  verboseIter = TRUE, allowParallel = TRUE))
    }
  })[3]
  print(ptime)
  
  return(my_model)
}

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

MultiLogLoss(my_test$interest, my_test$gbm_pred$high, my_test$gbm_pred$medium, my_test$gbm_pred$low)
MultiLogLoss(my_test$interest, my_test$ranger_pred$high, my_test$ranger_pred$medium, my_test$ranger_pred$low)
MultiLogLoss(my_test$interest, my_test$glmnet_pred$high, my_test$glmnet_pred$medium, my_test$glmnet_pred$low)

# Use created year, month, day, manager_id, manager_skill and manager fractionts, number of photos, number of features, 
# number of description words

# R starter
https://www.kaggle.com/gospursgo/two-sigma-connect-rental-listing-inquiries/h2o-starter-pack/run/835757

# Manager_id
https://www.kaggle.com/den3b81/two-sigma-connect-rental-listing-inquiries/do-managers-matter-some-insights-on-manager-id
https://www.kaggle.com/den3b81/two-sigma-connect-rental-listing-inquiries/improve-perfomances-using-manager-features
https://www.kaggle.com/aikinogard/two-sigma-connect-rental-listing-inquiries/random-forest-starter-with-numerical-features

# Building_id
https://www.kaggle.com/den3b81/two-sigma-connect-rental-listing-inquiries/some-insights-on-building-id

# XGBoost
https://cran.r-project.org/web/packages/xgboost/vignettes/xgboostPresentation.html

# Sentiment analysis
https://www.kaggle.com/psilogram/two-sigma-connect-rental-listing-inquiries/sentiment-analysis

