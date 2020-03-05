
NNetOneSplit <- function( X.mat, y.vec, max.epochs, step.size, n.hidden.units, is.subtrain )
{
  #intialization
  n.features <- ncol( X.mat )
  n.obs <- nrow( X.mat )
  
  #scale and split
  X.mat <- scale( X.mat )
  
  #X.subtrain and y.subtrain are the training data
  X.subtrain <- X.mat[ is.subtrain ]
  y.subtrain <- y.vec[ is.subtrain ]
  n.train.obs <- length( y.vec )
  
  #X.validation and y.validation is the test/validation data
  X.validation <- X[ !is.subtrain ]
  y.validation <- X[ !is.subtrain ]
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
      
      #calculate gradients for V.mat
      
      #calculate gradient for w.vec
      
    }
    
  }



} 

  ForwardProp<- function(observation.mat,weight.vec){
    #list of hidden units starts with h(0) the observations. 
    hiddenUnits.list<-list(observations.mat)
    # for each layer calculate the hidden units given activation function for each observation.
    for (layer.i in 1:lenght(weight.vec)){
      # for given layer matrix multiply weights and previous hidden units or observations to start.
      activation.vec <- weight.vec[[layer.i]]%*%hiddenUnits.list[[layer.i]]
      #if not at the last layer activation is sigmoid
      if(layer.i!= length(weight.vec)){
    
        hiddenUnits.list[[layer.i+1]]<- (1/1+exp(-activation.vec))
      
        }else{
          # use identity for last
          hiddenUnits.list[[layer.i+1]]<- a.vec 
      }
      
    }
    hiddenUnits.list
  }  
  
  BackProp<- function(hiddenUnits.list,weight.mat,ytilde){  
    #loop over the layers in the weight.matrix from larges to smallest
    for (layer.i in length(weight.mat):1) {
      #implement rule 0 to get gradient of layer from weight.mat
     if(layer.i== length(weight.mat)){
       gradient.a <-(-ytilde/(1+exp(ytilde*hiddenUnits.list[[lenght(hiddenUnits.list)]])))
     }else{#rule 3,2,1 
       #rule 3
       gradient.h<- t(weight.mat[[layer.i+1]]) %*% gradient.a
       hiddenUnits.vec <- hiddenUnits.list[[layer.i+1]]
       gradient.h * hiddenUnits.vec *(1-h.vec)
       }
      
      gradient.list<-list()
      #return a list of gradients
      gradient.list[[layer.i]]<- gradient.a%*%t(hiddenUnits.list[[layer.i]])
    }
    gradient.list
  }
  
  