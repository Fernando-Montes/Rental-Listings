# Load packages and data
setwd("~/Dropbox/Courses/Kaggle/Two-sigma-Rental")
library(jsonlite)
library(dplyr)
library(purrr)
library(knitr)
library(data.table) 
library(ggmap)
register_google(key = "....")

train <- fromJSON("Data/train.json")
# Set all variable names except `photos`` and `feactures`` into an array
column <- setdiff(names(train), c("photos", "features"))
# Unlist every variable except `photos` and `features` and convert to tibble
train <- map_at(train, column, unlist) %>% tibble::as_tibble(.)

# Creating dataframe of the variables to use in model
neigh.features <- data.frame(listing_id = character(0), 
                             neighborhood = character(0), 
                             political = character(0), stringsAsFactors=FALSE)

i <- 1
while ( i<=dim(train)[1]  ) {
  #while ( i<=1  ) {
  neigh.features[i,1] <- train$listing_id[i]
  temp <- revgeocode(location = c(train$longitude[i], train$latitude[i]), output = "more", override_limit = TRUE)
  try(neigh.features[i,2] <- as.character(temp$neighborhood), silent = TRUE)
  try(neigh.features[i,3] <- as.character(temp$political), silent = TRUE)
  print(i)
  i <- i+1 
}

# Saving manager.features into an .RData file
save(neigh.features, file = "RData/neighFeaturesTrain.RData")
load(file = "RData/neighFeaturesTrain.RData")
