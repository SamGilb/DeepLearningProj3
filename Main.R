library("data.table")
source("NNetOneSplit.R")
library("ggplot2")

set.seed(1)

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
n.test.obs <- length(y.test)
is.subtrain <- sample(rep(c(TRUE, TRUE, TRUE, FALSE, FALSE), ceiling(n.train.obs/5)), n.train.obs)

#run this boiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
NNetList <- NNetOneSplit( X.train, y.train, max.epochs = 30, step.size = 0.1, n.hidden.units = 20, is.subtrain )

train.log.loss <- NNetList[[1]][[1]]
validation.log.loss <- NNetList[[1]][[2]]

#print log loss with respect to epochs
plot<-ggplot()+
  labs(x="Epochs", y="LogLoss",color="set")+
  geom_path(aes(x=1:length(train.log.loss),y=train.log.loss,color= "train"))+
  geom_path(aes(x=1:length(validation.log.loss),y=validation.log.loss,color= "validation"))+
  geom_point(aes(x=which.min(validation.log.loss),y=validation.log.loss[which.min(validation.log.loss)],color="validationlossmin"))
print(plot)

best_epochs <- which.min(validation.log.loss)

#use NNetOneSplit with best_epochs and all of training data
NNetListCV <- NNetOneSplit( X.train, y.train, max.epochs = best_epochs, step.size = 0.1, n.hidden.units = 20, is.subtrain = rep(TRUE,length(is.subtrain)) )

#extract V.mat and w.vec
V.mat <- NNetListCV[[2]]
w.vec <- NNetListCV[[3]]

pred.test.y <- t(t(w.vec) %*% sigmoid(t(V.mat) %*% t(X.test)))
pred.test.label <- ifelse( pred.test.y >0, 1, 0)

model.accuracy <- sum(pred.test.label == y.test) / length(y.test)
print(model.accuracy)

baseline.accuracy <- sum(y.test == 1) / length(y.test)
