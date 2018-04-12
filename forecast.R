#функция для построения символьного представления функции
# вида output~X1+X2+X3+X4+X5
# количество X определяется параметром length
getTrainFormula <- function(length)
{
  cnames <- paste("X",1:length, sep="")
  paste("output",paste(cnames, collapse='+'), sep="~")
  
}

#функция построения обучающих данных без скольжения
makeTrainData0 <- function(input, tupleLength=5)
{
  len <- nrow(input)
  tupleCount <- len %/% tupleLength
  if (length(input)>0) {
    hm <- matrix(nrow=tupleCount,ncol=tupleLength+1)
    for (i in 1:tupleCount)
    {
      for (j in 1:tupleLength)
      {
        hm[i,j] <- input[1+(i-1)*tupleLength + j-1,1]
      }
      if (1+(i)*tupleLength<=len)
        hm[i,tupleLength+1] <- input[1+(i)*tupleLength,1]
    }  
    cnames <- vector(length=tupleLength)
    for (j in 1:tupleLength)
      cnames[j] <- j
    colnames(hm) <- c(cnames, "output")
    data.frame(hm)
  }
}

#функция построения обучающих данных со скользящим окном
#input - входной вектор, tupleLength - размер окна
makeTrainData <- function(input, tupleLength=5)
{
  if (nrow(input)>tupleLength-1)
  {
    tupleCount <- nrow(input)-tupleLength
    

    hm <- matrix(nrow=tupleCount,ncol=tupleLength+1)
    for (i in 1:tupleCount)
    {
      for (j in 1:tupleLength)
      {
        hm[i,j] <- input[1+(i-1)+(j-1),1]
      }
      hm[i,tupleLength+1] <- input[1+(i-1)+tupleLength,1]
    }  

    cnames <- vector(length=tupleLength)
    for (j in 1:tupleLength)
      cnames[j] <- j
    colnames(hm) <- c(cnames, "output")
    
    data.frame(hm)
  }
}

#функция обучения
train <- function(trainingData)
{
  neuralnet(getTrainFormula(ncol(trainingData)-1), data=trainingData, hidden = c(10,10), threshold=0.01)
}

norm.func <- function(x){ 
  data.frame((x - min(x))/(max(x) - min(x)))
  #data.frame(x/max(x))
}

mscale <- function(x, from_min, from_max)
{
  (as.matrix(x)-from_min)/(from_max-from_min)
  #as.matrix(x)/from_max
}

mrescale <- function(x, min, max)
{
  x*(max-min)+min
  #x*max
}

#итоговая функция, в которую отправляются исходные данные: data_dollar или data_euro
#параметр skyline - период прогнозирования от конца фактических данных
forecast <- function(x, skyline=15)
{
  trainingData <- makeTrainData(x)
  n <- train(norm.func(trainingData))
    
  for (i in 1:skyline)    
  {
    res <- compute(n,mscale(trainingData[nrow(trainingData),2:6],min(trainingData),max(trainingData)))$net.result 

    h <- cbind(data.frame(trainingData[nrow(trainingData),2:6]),mrescale(res,min(trainingData),max(trainingData)))
    colnames(h) <- colnames(trainingData)
    trainingData <- rbind(trainingData,h)
  }

  plot(trainingData[,6], col="red")
  points(x)
  #data.frame(trainingData)
}