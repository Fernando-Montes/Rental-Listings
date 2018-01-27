# Based on
# https://dahtah.github.io/imager/imager.html
setwd("~/Dropbox/Courses/Kaggle/Two-sigma-Rental")

# Creating matrix with one row per photo --------------
library(imager)
# Creating dataframe of the photos (one photo per row)
photos.pixel <- data.frame(matrix(vector(), 0, 1+40*40), stringsAsFactors=FALSE)

# Using photos.info dataframe (url and interest level) created in CreatePhotoDataFrame.R
load(file = "RData/photosInfo.RData")

for (i in 1:1000) {
  print(i)
  link <- photos.info[i,1]  # Using the url link
  download.file(link, basename(link))   # Download image file to local disk
  im <- load.image(basename(link))    # Load photo
  im.small <- resize(im, 40, 40) # Resize picture to 40x40 pixels
  im.gray <- grayscale(im.small)  # convert to gray-scale
  if (photos.info[i,2] == "low") interest <- 0
  else if (photos.info[i,2] == "medium") interest <- 1
  else interest <- 2
  photos.pixel[i,] <- cbind(interest, t(im.gray[1:1600]))  # Store pixel information one row at a time
  if (file.exists(basename(link))) file.remove(basename(link)) # Remove image file from local disk
}

# Saving photos.pixel into an .RData file
save(photos.pixel, file = "RData/photosPixel.RData")

# Processing photos.pixel --------------------
library (mxnet)

# Set up train and test datasets
train_x <- t(photos.pixel[, -1])  # Transpose all pixel information
train_y <- photos.pixel[, 1]  # Labels
train_array <- train_x
dim(train_array) <- c(40, 40, 1, ncol(train_x))

# Set up the symbolic model
data <- mx.symbol.Variable('data')
# 1st convolutional layer
conv_1 <- mx.symbol.Convolution(data = data, kernel = c(5, 5), num_filter = 50)
tanh_1 <- mx.symbol.Activation(data = conv_1, act_type = "tanh")
pool_1 <- mx.symbol.Pooling(data = tanh_1, pool_type = "max", kernel = c(1, 1), stride = c(1, 1))
# 2nd convolutional layer
conv_2 <- mx.symbol.Convolution(data = pool_1, kernel = c(2, 2), num_filter = 20)
tanh_2 <- mx.symbol.Activation(data = conv_2, act_type = "tanh")
pool_2 <- mx.symbol.Pooling(data=tanh_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
# 3rd convolutional layer
conv_3 <- mx.symbol.Convolution(data = pool_2, kernel = c(3, 3), num_filter = 50)
tanh_3 <- mx.symbol.Activation(data = conv_3, act_type = "tanh")
pool_3 <- mx.symbol.Pooling(data = tanh_3, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
# 4th convolutional layer
conv_4 <- mx.symbol.Convolution(data = pool_3, kernel = c(5, 5), num_filter = 20)
tanh_4 <- mx.symbol.Activation(data = conv_4, act_type = "tanh")
pool_4 <- mx.symbol.Pooling(data=tanh_4, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
# 1st fully connected layer
flatten <- mx.symbol.Flatten(data = pool_4)
fc_1 <- mx.symbol.FullyConnected(data = flatten, num_hidden = 500)
tanh_5 <- mx.symbol.Activation(data = fc_1, act_type = "tanh")
# 2nd fully connected layer
fc_2 <- mx.symbol.FullyConnected(data = tanh_5, num_hidden = 3)
# Output. Softmax output since we'd like to get some probabilities.
NN_model <- mx.symbol.SoftmaxOutput(data = fc_2)

# Pre-training set up
# Set seed for reproducibility
mx.set.seed(100)
# Device used. CPU in my case.
devices <- mx.cpu()

# Train the model
model <- mx.model.FeedForward.create(NN_model,
                                     X = train_array,
                                     y = train_y,
                                     ctx = devices,
                                     num.round = 480,
                                     array.batch.size = 40,
                                     learning.rate = 0.01,
                                     momentum = 0.9,
                                     eval.metric = mx.metric.accuracy,
                                     epoch.end.callback = mx.callback.log.train.metric(100))

