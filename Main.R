library("data.table")
source("NNetOneSplit.R")

#import data as list
data <- fread("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.data")
#convert data to matrix
data.matrix <- matrix(unlist(data), nrow(data), ncol(data))

#separate into corresponding data
X.sc <- scale(data.matrix[,1:57])
y <- data.matrix[,58]

#set up is.train vec
n.obs <- length(y) #4601
is.test <- sample(rep(c(TRUE,FALSE,FALSE,FALSE,FALSE),ceiling(n.obs/5)), n.obs)

#set up train and test data
X.test <- X.sc[is.test,]
X.train <- X.sc[!is.test,]
y.test <- y[is.test]
y.train <- y[!is.test]

#set up is.subtrain vector
n.train.obs <- length(y.train)
is.subtrain <- sample(rep(c(TRUE, TRUE, TRUE, FALSE, FALSE), ceiling(n.train.obs/5)), n.train.obs)

NNetList <- NNetOneSplit( X.train, y.train, max.epochs = 5, step.size = 0.1, n.hidden.units = 20, is.subtrain )




