# Based on
# https://www.kaggle.com/ygtcrt/two-sigma-connect-rental-listing-inquiries/how-to-deal-with-features-in-renthop-data/discussion

# Load packages and data
setwd("~/Dropbox/Courses/Kaggle/Two-sigma-Rental")
library(jsonlite)
library(dplyr)
library(purrr)
library(knitr)
library(stringr)

train <- fromJSON("Data/train.json")
# Set all variable names except `photos`` and `features`` into an array
column <- setdiff(names(train), c("photos", "features"))
# Unlist every variable except `photos` and `features` and convert to tibble
train <- map_at(train, column, unlist) %>% tibble::as_tibble(.)

# Total number of feature in train set
length(unique(train$features))

# Summarize count of features
feature = data.frame(feature = tolower(unlist(train$features))) %>% # convert all features to lower case
  group_by(feature) %>%
  summarise(feature_count = n()) %>%
  arrange(desc(feature_count)) %>%
  filter(feature_count >= 1)

kable(feature, caption = "Feature Count")

# Finding homogenezing features
# ---------
new.features <- data.frame(matrix(vector(), 0, 2, dimnames=list(c(),c("Features", "Exclude"))), stringsAsFactors=F)

# Elevator -----
new.features[1,1] <- "elevator"
new.features[1,2] <- "----"
feature %>% filter(str_detect(feature, new.features[1,1])) %>% filter(!str_detect(feature, new.features[1,2])) %>% kable()

# Wood -----
new.features[2,1] <- "wood"
new.features[2,2] <- "fireplace"
feature %>% filter(str_detect(feature, new.features[2,1])) %>% filter(!str_detect(feature, new.features[2,2])) %>% kable()

# Pets -----
new.features[3,1] <- paste(c('cats', "dogs", "pet"), collapse="|")
new.features[3,2] <- paste(c("no pets", "competitively"), collapse="|")
feature %>% filter(str_detect(feature, new.features[3,1])) %>% filter(!str_detect(feature, new.features[3,2])) %>% kable()

# NoPets -----
new.features[4,1] <- "no pets"
new.features[4,2] <- "----"
feature %>% filter(str_detect(feature, new.features[4,1])) %>% filter(!str_detect(feature, new.features[4,2])) %>% kable()

# Doorman -----
new.features[5,1] <- "doorman"
new.features[5,2] <- "----"
feature %>% filter(str_detect(feature, new.features[5,1])) %>% filter(!str_detect(feature, new.features[5,2])) %>% kable()

# Dishwasher -----
new.features[6,1] <- paste(c('dishwasher', "dish washer"), collapse = "|")
new.features[6,2] <- "----"
feature %>% filter(str_detect(feature, new.features[6,1])) %>% filter(!str_detect(feature, new.features[6,2])) %>% kable()

# LaundryUnit  ----
new.features[7,1] <- paste(c('laundry', 'dryer', 'washer'), collapse="|")
new.features[7,2] <- paste(c('dishwasher', 'in building', "on site", "floor", "on-site", "dish washer", "in bldg"), collapse="|")
feature %>% filter(str_detect(feature, new.features[7,1])) %>% filter(!str_detect(feature, new.features[7,2])) %>% kable()

# LaundryBuilding ----
new.features[8,1] <- paste(c('laundry in building', "laundry in bldg", "laundry in building", "washer/dryer in bldg", 
                             "on-site laundry", "washer/dryer in building"), collapse="|")
new.features[8,2] <- "----"
feature %>% filter(str_detect(feature, new.features[8,1])) %>% filter(!str_detect(feature, new.features[8,2])) %>% kable()

# prewar ----
new.features[9,1] <- paste(c('pre-war',"prewar"), collapse="|")
new.features[9,2] <- "----"
feature %>% filter(str_detect(feature, new.features[9,1])) %>% filter(!str_detect(feature, new.features[9,2])) %>% kable()

# roofdeck ----
new.features[10,1] <- paste(c('roof',"deck"), collapse="|")
new.features[10,2] <- "----"
feature %>% filter(str_detect(feature, new.features[10,1])) %>% filter(!str_detect(feature, new.features[10,2])) %>% kable()

# OutdoorPublic ----
new.features[11,1] <- paste(c("outdoor", "residents garden", "yard"), collapse="|")
new.features[11,2] <- paste(c('private', "pool"), collapse="|")
feature %>% filter(str_detect(feature, new.features[11,1])) %>% filter(!str_detect(feature, new.features[11,2])) %>% kable()

# OutdoorPrivate ----
new.features[12,1] <- paste(c('garden', 'patio', "private"), collapse="|")
new.features[12,2] <- paste(c('residents', "common", "shared"), collapse="|")
feature %>% filter(str_detect(feature, new.features[12,1])) %>% filter(!str_detect(feature, new.features[12,2])) %>% kable()

# Diningroom ----
new.features[13,1] <- 'dining room'
new.features[13,2] <- "----"
feature %>% filter(str_detect(feature, new.features[13,1])) %>% filter(!str_detect(feature, new.features[13,2])) %>% kable()

# Internet ----
new.features[14,1] <- paste(c('internet',"wifi", "wi fi"), collapse = "|")
new.features[14,2] <- "----"
feature %>% filter(str_detect(feature, new.features[14,1])) %>% filter(!str_detect(feature, new.features[14,2])) %>% kable()

# Balcony ----
new.features[15,1] <- paste(c('balcony', "terrace"), collapse="|")
new.features[15,2] <- "----"
feature %>% filter(str_detect(feature, new.features[15,1])) %>% filter(!str_detect(feature, new.features[15,2])) %>% kable()

# Pool ----
new.features[16,1] <- 'pool'
new.features[16,2] <- "----"
feature %>% filter(str_detect(feature, new.features[16,1])) %>% filter(!str_detect(feature, new.features[16,2])) %>% kable()

# NoFee ----
new.features[17,1] <- paste(c('no fee',"reduced fee"), collapse="|")
new.features[17,2] <- "----"
feature %>% filter(str_detect(feature, new.features[17,1])) %>% filter(!str_detect(feature, new.features[17,2])) %>% kable()

# Fitness ----
new.features[18,1] <- paste(c("fitness", "gym", "health", "recreation", "bike room"), collapse="|")
new.features[18,2] <- "----"
feature %>% filter(str_detect(feature, new.features[18,1])) %>% filter(!str_detect(feature, new.features[18,2])) %>% kable()

# new construction ----
new.features[19,1] <- 'new constru'
new.features[19,2] <- "----"
feature %>% filter(str_detect(feature, new.features[19,1])) %>% filter(!str_detect(feature, new.features[19,2])) %>% kable()

# exclusive ----
new.features[20,1] <- paste(c('exclusive', 'luxury', "concierge", "marble bath", "valet", "sauna"), collapse="|")
new.features[20,2] <- "----"
feature %>% filter(str_detect(feature, new.features[20,1])) %>% filter(!str_detect(feature, new.features[20,2])) %>% kable()

# High ceilings ----
new.features[21,1] <- paste(c('ceiling', 'ceilings'), collapse="|")
new.features[21,2] <- "----"
feature %>% filter(str_detect(feature, new.features[21,1])) %>% filter(!str_detect(feature, new.features[21,2])) %>% kable()

# renovated ----
new.features[22,1] <- 'renovated'
new.features[22,2] <- "----"
feature %>% filter(str_detect(feature, new.features[22,1])) %>% filter(!str_detect(feature, new.features[22,2])) %>% kable()

# Park ----
new.features[23,1] <- paste(c('park', 'garage'), collapse = "|")
new.features[23,2] <- "----"
feature %>% filter(str_detect(feature, new.features[23,1])) %>% filter(!str_detect(feature, new.features[23,2])) %>% kable()

# livein ----
new.features[24,1] <- paste(c("super"), collapse = "|")
new.features[24,2] <- "----"
feature %>% filter(str_detect(feature, new.features[24,1])) %>% filter(!str_detect(feature, new.features[24,2])) %>% kable()

# lounge ----
new.features[25,1] <- paste(c('lounge', "lobby"), collapse = "|")
new.features[25,2] <- "----"
feature %>% filter(str_detect(feature, new.features[25,1])) %>% filter(!str_detect(feature, new.features[25,2])) %>% kable()

# closet ----
new.features[26,1] <- 'closet'
new.features[26,2] <- "----"
feature %>% filter(str_detect(feature, new.features[26,1])) %>% filter(!str_detect(feature, new.features[26,2])) %>% kable()

# playroom ----
new.features[27,1] <- 'playroom'
new.features[27,2] <- "----"
feature %>% filter(str_detect(feature, new.features[27,1])) %>% filter(!str_detect(feature, new.features[27,2])) %>% kable()

# ac ----
new.features[28,1] <- paste(c('a/c',"central","air cond"), collapse = "|")
new.features[28,2] <- "----"
feature %>% filter(str_detect(feature, new.features[28,1])) %>% filter(!str_detect(feature, new.features[28,2])) %>% kable()

# wheelchair ----
new.features[29,1] <- paste(c('wheelchair',"wheel-chair"), collapse="|")
new.features[29,2] <- "----"
feature %>% filter(str_detect(feature, new.features[29,1])) %>% filter(!str_detect(feature, new.features[29,2])) %>% kable()

# fireplace ----
new.features[30,1] <- 'fireplace'
new.features[30,2] <- "----"
feature %>% filter(str_detect(feature, new.features[30,1])) %>% filter(!str_detect(feature, new.features[30,2])) %>% kable()

# loft ----
new.features[31,1] <- 'loft'
new.features[31,2] <- "----"
feature %>% filter(str_detect(feature, new.features[31,1])) %>% filter(!str_detect(feature, new.features[31,2])) %>% kable()

# simplex ----
new.features[32,1] <- 'simplex'
new.features[32,2] <- "----"
feature %>% filter(str_detect(feature, new.features[32,1])) %>% filter(!str_detect(feature, new.features[32,2])) %>% kable()

# lowrise ----
new.features[33,1] <- paste(c('lowrise', "low-rise"), collapse="|")
new.features[33,2] <- "----"
feature %>% filter(str_detect(feature, new.features[33,1])) %>% filter(!str_detect(feature, new.features[33,2])) %>% kable()

# highrise ----
new.features[34,1] <- paste(c('highrise', "hi rise", "hirise", "high rise"), collapse="|")
new.features[34,2] <- "----"
feature %>% filter(str_detect(feature, new.features[34,1])) %>% filter(!str_detect(feature, new.features[34,2])) %>% kable()

# furnished ----
new.features[35,1] <- paste(c("furnished"), collapse="|")
new.features[35,2] <- "----"
feature %>% filter(str_detect(feature, new.features[35,1])) %>% filter(!str_detect(feature, new.features[35,2])) %>% kable()

# duplex ----
new.features[36,1] <- paste(c("duplex","multi-level", "multilevel"), collapse="|")
new.features[36,2] <- "----"
feature %>% filter(str_detect(feature, new.features[36,1])) %>% filter(!str_detect(feature, new.features[36,2])) %>% kable()

# green ----
new.features[37,1] <- paste(c("green", "eco"), collapse="|")
new.features[37,2] <- "----"
feature %>% filter(str_detect(feature, new.features[37,1])) %>% filter(!str_detect(feature, new.features[37,2])) %>% kable()

# storage ----
new.features[38,1] <- paste(c("storage"), collapse="|")
new.features[38,2] <- "----"
feature %>% filter(str_detect(feature, new.features[38,1])) %>% filter(!str_detect(feature, new.features[38,2])) %>% kable()

# ss ----
new.features[39,1] <- paste(c("stainless", "granite kitchen", "granite"), collapse="|")
new.features[39,2] <- "----"
feature %>% filter(str_detect(feature, new.features[39,1])) %>% filter(!str_detect(feature, new.features[39,2])) %>% kable()

# light ----
new.features[40,1] <- paste(c("light"), collapse="|")
new.features[40,2] <- "----"
feature %>% filter(str_detect(feature, new.features[40,1])) %>% filter(!str_detect(feature, new.features[40,2])) %>% kable()

# misc ----
new.features[41,1] <- paste(c("exposed brick", "eat in kitchen", "photos", "live/work", "microwave", 
                              "shares ok", "brownstone"), collapse="|")
new.features[41,2] <- "----"
feature %>% filter(str_detect(feature, new.features[41,1])) %>% filter(!str_detect(feature, new.features[41,2])) %>% kable()

# subway ----
new.features[42,1] <- paste(c("subway", "train"), collapse="|")
new.features[42,2] <- "----"
feature %>% filter(str_detect(feature, new.features[42,1])) %>% filter(!str_detect(feature, new.features[42,2])) %>% kable()

# shortTerm ----
new.features[43,1] <- paste(c("short term", "short-term", "seasonal"), collapse="|")
new.features[43,2] <- "----"
feature %>% filter(str_detect(feature, new.features[43,1])) %>% filter(!str_detect(feature, new.features[43,2])) %>% kable()

# View ----
new.features[44,1] <- paste(c("view"), collapse="|")
new.features[44,2] <- "----"
feature %>% filter(str_detect(feature, new.features[44,1])) %>% filter(!str_detect(feature, new.features[44,2])) %>% kable()

# postwar ----
new.features[45,1] <- paste(c("post-war"), collapse="|")
new.features[45,2] <- "----"
feature %>% filter(str_detect(feature, new.features[45,1])) %>% filter(!str_detect(feature, new.features[45,2])) %>% kable()

# -----

# Saving new.features into an .RData file
save(new.features, file = "RData/newFeatures.RData")

# Identifying features not yet catalogued
i <- 0
found.new <- FALSE
while(i < dim(feature)[1] & !found.new) {
  i <- i+1
  j <- 0
  while(j < dim(new.features)[1] & !found.new) {
    j <- j+1
    if( str_detect(feature$feature[i], new.features[j,1]) & !str_detect(feature$feature[i], new.features[j,2]) ) {
      found.new <- TRUE
    }
  }
  if (found.new == FALSE) found.new <- TRUE
  else found.new <- FALSE
}
feature[i,]
