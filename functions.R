step2 <- function(x)
{
  if (length(x)<2) x
  minDist=Inf
  #h <- c(min(x),max(x))
  for (i in seq(from=1,to=length(x)-1))
    for (j in seq(from=i+1, to=length(x)))
    {
      if (abs(x[i]-x[j])<minDist) {
        minDist = abs(x[i]-x[j])
        h <- c(i,j)
      }
    }
  
  out <- vector(length=length(x)-1)
  
}

#x - входные кластеры
#length - длина x
step <- function(x, length)
{
  if (length<1) x
  minDist <- Inf
  h <- 0
  
  for (i in seq(from=1,to=length-1))
    {
      if (x[i+1]-x[i]<minDist) {
        minDist = abs(x[i+1]-x[i])
        h <- i
      }
    }

  x[h] <- (x[h+1]-x[h])/2+x[h]

  if (h<length-1)
  for (i in seq(from=h+1,to=length-1))
    x[i] <- x[i+1]
  x[length] <- NaN

  x
}


do <- function(x)
{
  x <- sort(x)
  dataout <- matrix(x, ncol=length(x))
  
  for (i in seq(from=1, to=length(x)-1))
    dataout <- rbind(dataout,step(dataout[i,1:length(x)],length(x)-i+1))
  
  dataout
}