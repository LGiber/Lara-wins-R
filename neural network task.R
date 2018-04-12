euro <- read.csv('euro.csv')
data_euro <- data.frame(euro)

#number of hidden neurons

nn.sizes <- c(4,2,3,3,3,2,2,2)


numofsubs <- length(data_euro)
twindow <- 4

offsettedsubdfs <- lapply(1:numofsubs, function(x){
  singleoffsets <- lapply(0:(twindow-1), function(y){
    data_euro[[x]][(twindow-y):(length(data_euro[[x]])-y-1)]
  })
  a <- Reduce(cbind, singleoffsets)
  names <- lapply(1:twindow, function(y){paste("TS", as.character(x), "_", as.character(y), sep = "")})
  b <- as.data.frame(a)
  colnames(b) <- names
  b
})

sample.number <- length(offsettedsubdfs[[1]][,1])

#the neural networks

nns <- lapply(1:length(offsettedsubdfs), function(i)
  
{
  
  nn <- nnet(offsettedsubdfs[[i]][1:(sample.number),], #the training samples
             
             data_euro[[i]][(twindow+1):(length(data_euro[[i]]))], #the output
             
             #corresponding to the training samples
             
             size=nn.sizes[i], #number of neurons
             
             maxit = 1000, #number of maximum iteration
             
             linout = TRUE) #the neuron in the output layer should be linear
  
  #the result of the trained networks should be plotted
  
  plot(data_euro[[i]][(twindow+1):(length(data_euro[[i]]))], type="l")
  
  lines(nn$fitted.values,type="l",col="red")
  
  nn
  
})



number.of.predict <- 14

#long term prediction

long.predictions <- lapply(1:length(offsettedsubdfs), function(i)
  
{
  
  prediction <- vector(length=number.of.predict, mode="numeric")
  
  #initial input
  
  input <- offsettedsubdfs[[i]][sample.number,]
  
  for (j in 1 : number.of.predict)
    
  {
    
    prediction[j] <- predict(nns[[i]], input)
    
    input <- c(prediction[j],input[1:(length(input)-1)])
    
  }
  
  #we want to plot the prediction
  
  plot(c(nns[[i]]$fitted.values,prediction), type="l",col="red")
  
  lines(data_euro[[i]][(twindow+1):length(data_euro[[i]])])
  
  prediction
  
})