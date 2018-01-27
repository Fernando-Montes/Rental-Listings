# Load packages and data
setwd("~/Dropbox/Courses/Kaggle/Two-sigma-Rental")
library(jsonlite)
library(dplyr)
library(purrr)
library(knitr)
library(stringr)
library(data.table) 

train <- fromJSON("Data/train.json")
# Set all variable names except `photos`` and `feactures`` into an array
column <- setdiff(names(train), c("photos", "features"))
# Unlist every variable except `photos` and `features` and convert to tibble
train <- map_at(train, column, unlist) %>% tibble::as_tibble(.)

diffmanagers <- unique(train$manager_id)

manager.features <- data.frame(manager_id = character(0), 
                               managerNoLis = numeric(0),
                               managerLoPer = numeric(0),
                               managerMePer = numeric(0),
                               managerHiPer = numeric(0),
                               managerSkill = numeric(0), stringsAsFactors=FALSE)


# Loop over all different managers
i <- 1
while ( i<=length(diffmanagers)  ) {
  manager.features[i,1] = diffmanagers[i]
  manager.features$managerNoLis[i] = dim(train[train$manager_id == diffmanagers[i],])[1]
  manager.features$managerLoPer[i] = dim(train[train$manager_id == diffmanagers[i] & train$interest_level == "low",])[1]/dim(train[train$manager_id == diffmanagers[i] ,])[1]
  manager.features$managerMePer[i] = dim(train[train$manager_id == diffmanagers[i] & train$interest_level == "medium",])[1]/dim(train[train$manager_id == diffmanagers[i] ,])[1]
  manager.features$managerHiPer[i] = dim(train[train$manager_id == diffmanagers[i] & train$interest_level == "high",])[1]/dim(train[train$manager_id == diffmanagers[i] ,])[1]
  manager.features$managerSkill[i] = manager.features$managerMePer[i] + 2.0*manager.features$managerHiPer[i]
  i <- i+1
}
# Add percertanges for the average manager
manager.features[i,1] = "Average"
manager.features$managerLoPer[i] = dim(train[train$interest_level == "low",])[1]/dim(train)[1]
manager.features$managerMePer[i] = dim(train[train$interest_level == "medium",])[1]/dim(train)[1]
manager.features$managerHiPer[i] = dim(train[train$interest_level == "high",])[1]/dim(train)[1]
manager.features$managerSkill[i] = manager.features$managerMePer[i] + 2.0*manager.features$managerHiPer[i]

# Saving manager.features into an .RData file
save(manager.features, file = "RData/ManagerFeatures.RData")