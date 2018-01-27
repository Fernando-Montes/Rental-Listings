# Load packages and data
setwd("~/Dropbox/Courses/Kaggle/Two-sigma-Rental")
# Loading libraries
library(ggplot2)
library(data.table)
library(ggmap)

# Loading train data with feature engineering
load(file = "RData/TrainProcessed.RData")

# Univariate features
prop.table(table(t1$interest))

prop.table(table(t1$exclusive))

# countFeatures - interest
ggplot(t1, aes(countFeatures, fill = interest)) + geom_bar(position="fill")

# histogram price
ggplot(data=t1, aes(t1$price)) + geom_histogram(breaks=seq(20, 5000, by = 200), col="red", aes(fill=..count..))
# price - interest
ggplot(data=t1, aes(t1$price, fill = interest)) + geom_histogram(breaks=seq(20, 10000, by = 300), position="fill")

# bedrooms vs price
ggplot(data=t1, aes(x = t1$price, y = t1$bedrooms)) + geom_count()  +  xlim(0, 10000)  + geom_smooth(method="lm")

# histogram price/bedrooms
ggplot(data=t1, aes(t1$price/t1$bedrooms)) + geom_histogram(breaks=seq(20, 5000, by = 200), col="red", aes(fill=..count..))

# price/bedrooms - interest   --------------- ADD price/bedrooms FEATURE!!!!!!!!!
ggplot(data=t1, aes(t1$price/t1$bedrooms, fill = interest)) + geom_histogram(breaks=seq(20, 5000, by = 200), position="fill")

# price/countFeatures - interest  --------------- ADD price/countFeature FEATURE!!!!!!!!!
ggplot(data=t1, aes(t1$price/t1$countFeatures, fill = interest)) + geom_histogram(breaks=seq(20, 5000, by = 200), position="fill")

# histogram longitude
ggplot(data=t1, aes(t1$longitude)) + geom_histogram( breaks=seq(-74.1, -73.6, by = 0.005), col="red", aes(fill=..count..) )
# histogram latitude
ggplot(data=t1, aes(t1$latitude)) + geom_histogram(  breaks=seq(40.5, 41, by = 0.005), col="red", aes(fill=..count..) )

# latitude vs longitude
ggplot(data=t1, aes(x = t1$longitude, y = t1$latitude)) + geom_count(aes(col=interest)) + xlim(-74.1, -73.65) + ylim(40.5, 41)

# Plotting map
# Get New York longitude and latitude --
newyork <-  geocode("newyork")  # get longitude and latitude
# Google Satellite Map
newyork_ggl_sat_map <- qmap("newyork", zoom=11, source = "google", maptype="satellite")  
newyork_ggl_sat_map + geom_count(data = t1, aes(x=t1$longitude, y=t1$latitude, col=interest)) 

newyork_ggl_sat_map + geom_count(data = t1, aes(x=t1$longitude, y=t1$latitude, col=t1$price/t1$bedrooms)) + 
  scale_color_continuous(limits = c(0,5000))

BrooklynMap <- get_map(location = "Brooklyn", zoom=11)
ggmap(BrooklynMap) + geom_count(data = t1, aes(x=t1$longitude, y=t1$latitude, col=interest)) 

# wday - interest
ggplot(data=t1, aes(t1$wday, fill = interest)) + geom_histogram( bins = 7)
ggplot(data=t1, aes(t1$wday, fill = interest)) + geom_histogram( position = "fill", bins = 7)

# yday - interest
ggplot(data=t1, aes(t1$yday, fill = interest)) + geom_histogram()
ggplot(data=t1, aes(t1$yday, fill = interest)) + geom_histogram( position = "fill" )

# hour - interest
ggplot(data=t1, aes(t1$hour, fill = interest)) + geom_histogram( bins = 24 )
ggplot(data=t1, aes(t1$hour, fill = interest)) + geom_histogram( position = "fill", bins = 24)

# hour vs price/bedrooms
ggplot(data=t1, aes(x = t1$price/t1$bedrooms, y = t1$hour)) + geom_count(aes(col=interest)) + xlim(0, 5000)

# price.bedrooms.perNeigh - interest ---- GOOD FEATURE!!!!!!!!
ggplot(data=t1, aes(price.bedrooms.perNeigh, fill = interest)) + geom_histogram( ) + xlim(0, 4)
ggplot(data=t1, aes(price.bedrooms.perNeigh, fill = interest)) + geom_histogram( position = "fill" ) + xlim(0, 4)

# price.bedrooms.perPolit - interest ---- GOOD FEATURE!!!!!!!!
ggplot(data=t1, aes(price.bedrooms.perPolit, fill = interest)) + geom_histogram( ) + xlim(0, 4)
ggplot(data=t1, aes(price.bedrooms.perPolit, fill = interest)) + geom_histogram( position = "fill" ) + xlim(0, 4)

# price.bedrooms.perNeigh vs price.bedrooms
ggplot(data=t1, aes(x = price.bedrooms, y = price.bedrooms.perNeigh)) + geom_count(aes(col=interest)) + xlim(0, 5000)

# price.countFeatures.perPolit - interest ---- GOOD FEATURE!!!!!!!!
ggplot(data=t1, aes(price.countFeatures.perNeigh, fill = interest)) + geom_histogram( ) + xlim(0, 4)
ggplot(data=t1, aes(price.countFeatures.perNeigh, fill = interest)) + geom_histogram( position = "fill" ) + xlim(0, 4)

# price.bedrooms vs price.countFeatures
ggplot(data=t1, aes(x = price.bedrooms, y = price.countFeatures)) + geom_count(aes(col=interest)) + xlim(0, 5000) + ylim(0, 5000)
ggplot(data=t1, aes(x = price.bedrooms.perNeigh, y = price.countFeatures.perNeigh)) + geom_count(aes(col=interest)) + xlim(0, 4) + ylim(0, 4)

# price vs countFeatures
ggplot(data=t1[t1$neighborhood == "Lower Manhattan" & t1$bedrooms == 3,], aes(x = price, y = countFeatures)) + geom_count(aes(col=interest)) + xlim(0, 5000) + ylim(0, 30)

# numListings - interest
ggplot(data=t1[t1$neighborhood == "Lower Manhattan",], aes(numListings.5.10, fill = interest)) + geom_histogram( )
# numListings vs neighborhood
ggplot(data=t1[t1$political == "Manhattan",], aes(x = numListings.5.10, y = neighborhood)) + geom_count(aes(col=interest)) 
# numListings vs yday
ggplot(data=t1[t1$neighborhood == "Lower Manhattan" & t1$bedrooms == 2,], aes(x = yday, y = numListings.5.5)) + geom_point(aes(col=interest)) 

# -------------------------------
# Loading train-test data with comparison features included
load(file = "RData/AllData.RData")
t1 <- AllData[1:49352,]

# numListings vs yday
ggplot(data=t1[t1$neighborhood == "Lower Manhattan" & t1$bedrooms == 2,], aes(x = yday, y = numListings.perNeigh.0)) + geom_count(aes(col=interest)) 
ggplot(data=t1[t1$political == "Manhattan" & t1$bedrooms == 2,], aes(x = yday, y = numListings.perPolit.0.0)) + geom_count(aes(col=interest)) 

# price vs yday
ggplot(data=t1[t1$neighborhood == "Lower Manhattan" & t1$bedrooms == 2,], aes(x = yday, y = price.perNeigh.0.0)) + geom_count(aes(col=interest)) + ylim(-0,5)
ggplot(data=t1[t1$political == "Manhattan" & t1$bedrooms == 2,], aes(x = yday, y = price.perPolit.0.0)) + geom_count(aes(col=interest)) + ylim(-0,5)

# numListings - interest
ggplot(data=t1[t1$neighborhood == "Lower Manhattan" & t1$bedrooms == 2,], aes(numListings.perNeigh.5, fill = interest)) + geom_histogram(position = "fill", bins = 70 )

# price - interest
ggplot(data=t1[t1$neighborhood == "Lower Manhattan" & t1$bedrooms == 2,], aes(price.perNeigh.10, fill = interest)) + geom_histogram(position = "fill", bins = 70 ) + xlim(0,4)

# managerSkill - interest
ggplot(data=t1, aes(managerSkill, fill = interest)) + geom_histogram(position = "fill", bins = 70 ) 

# buildingSkill - interest
ggplot(data=t1, aes(buildingSkill, fill = interest)) + geom_histogram(position = "fill", bins = 70 ) 

# -------------------------------
# OTHER 
mean(t1[t1$latitude > 40.5 & t1$latitude < 40.7,"price"]/t1[t1$latitude > 40.5 & t1$latitude < 40.7,"bedrooms"], trim = .13)

revgeocode(location = c(t1[1,"longitude"], t1[1,"latitude"]), output = "more")
