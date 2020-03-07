library("sigmoid")

#Function: NNetOneSplit
#Algorithm: Assuming a Neural Network 
NNetOneSplit <- function( X.mat, y.vec, max.epochs, step.size, n.hidden.units, is.subtrain )
{
  #intialization
  n.features <- ncol( X.mat )
  n.obs <- nrow( X.mat )
  h.values <- numeric( n.hidden.units )
  pred.y.vec <- numeric()
  yt.vec <- numeric()
  pred.yt.vec <- numeric()
  mean.train.log.loss <- numeric()
  mean.validation.log.loss <- numeric()
  
  #scale and split
  X.mat <- scale( X.mat )
  
  #X.subtrain and y.subtrain are the training data
  X.subtrain <- X.mat[ is.subtrain, ]
  y.subtrain <- y.vec[ is.subtrain ]
  n.train.obs <- length( y.vec )
  
  #X.validation and y.validation is the test/validation data
  X.validation <- X.mat[ !is.subtrain, ]
  y.validation <- X.mat[ !is.subtrain ]
  n.validation.obs <- length( y.vec )
  
  #V.mat is a matrix (n.features x n.hidden.units) used to predict hidden units given inputs
  #initialize V.mat with values close to 0
  V.mat <- matrix( rnorm( n.features*n.hidden.units ), n.features, n.hidden.units )
  
  #w.vec is a weight vector (n.hidden.units) used to predict output given hidden units
  #initialize w.vec with values close to 0
  w.vec <- rnorm( n.hidden.units )
  
  for( k in 1:max.epochs )
  {
    
    #for every epoc, iterate through all training observations
    for( curr.train.obs in 1:n.train.obs )
    {
      #set aside current observation
      curr.y <- y.subtrain[ curr.train.obs ]
      curr.X <- X.subtrain[ curr.train.obs, ]
      
      #initialize y tilda
      curr.yt <- ifelse( curr.y == 1, 1, -1)
      #curr.yt <- if(curr.y == 1){1}else{-1}
      
      #########################################################################
      ##############################Propogation################################
      #########################################################################
      
      #Forward propogation
      h.values <- sigmoid( t(V.mat) %*% curr.X )
      pred.y <- t(h.values) %*% w.vec
      
      #the predicted label is going to be 1 if pred.y is positive, 0 otherwise.
      pred.label <- ifelse(pred.y > 0, 1, 0)
      #pred.label <- if(pred.y > 0){1}else{0}
      
      #calculate y tilda
      pred.yt <- ifelse(pred.y >0, 1, -1)
      #pred.yt <- if(pred.y > 0 ){1}else{-1}
      
      #record results for pred.y and yt for log loss plotting later
      pred.y.vec[curr.train.obs] <- pred.y
      pred.yt.vec[curr.train.obs] <- pred.yt
      
      #Back propogation
      w.vec.grad <- -pred.yt / (1 + exp(pred.yt * pred.y))
      print(dim(w.vec.grad))
      print(dim((h.values %*% w.vec.grad )))
      print(dim(((h.values %*% w.vec.grad) * h.values * h.values)))
      V.mat.grad <- (h.values %*% w.vec.grad ) %*% ((h.values %*% w.vec.grad) * h.values * h.values)
      
      #adjust weight matrices
      w.vec <- w.vec - step.size * w.vec.grad
      V.mat <- V.mat.grad - step.size * V.mat.grad
      
    }
    #end of epoch
    
    #calculate log loss for training
    pred.y <- h.values %*% w.vec
    pred.yt <- ifelse(pred.y >0, 1, -1)
    #pred.yt <- if(pred.y >0){1}else{-1}
    train.log.loss <- log(1 + exp(-pred.yt * pred.y))
    
    #loop through validation set and build validation.log.loss
    for( curr.validation.obs in 1:n.validation.obs )
    {
      #calculate pred.validation.y and pred.validation.yt
      pred.validation.y <- w.vec %*% (sigmoid(t(V.mat) %*% X.validation[curr.validation.obs]))
      pred.validation.y <- ifelse(pred.validation.y > 0, 1, -1)
      #pred.validation.yt <- if(pred.validation.y >0 ){1}else{-1}
      
      #store results in vector, later used for plotting
      pred.validation.y.vec[curr.validation.obs] <- pred.validation.y
      pred.validation.yt.vec[curr.validation.obs] <- pred.validation.yt
    }
    #calculating validation log loss over all data points
    validation.log.loss <- log(1 + exp(-pred.validation.yt.vec * pred.validation.y))
    
    #store the main log loss for this specific epoch
    mean.train.log.loss[k] <- mean(train.log.loss)
    mean.validation.log.loss[k] <- mean(validation.log.loss)
    
  }
  #end of training
  
  #create loss.values
  loss.values <- list(mean.train.log.loss,mean.validation.log.loss)
  
  #return
  list(loss.values, V.mat, w.vec)
}