kurs <- read.csv("C:/Users/home/Desktop/R/kurs.csv", sep=";")
kurs
OUT <- data.matrix(kurs[3:4])[,1]
OUT
kurS <- cbind(kurs[3:4], OUT)
kurS
In <- kurS[1:2]
In
Out <- kurS[3]
Out
fuzzySystem <- fugeR.run( In,
                          Out,
                          generation=100, # Increase the number of generation for a better accuracy
                          population=100,
                          elitism=20,
                          verbose=TRUE,
                          threshold=NA,
                          sensiW=0.0,
                          speciW=0.0,
                          accuW=0.0,
                          rmseW=1.0,
                          maxRules=5,
                          maxVarPerRule=2,
                          labelsMf=3
)

prediction <- fugeR.predict(fuzzySystem, In)


plot(prediction[[1]], ylim=c(1,max(unlist(Out))), col='blue', pch=21, axes=FALSE, ann=FALSE)
points(Out[[1]], col="red", pch=21)
axis(1)
axis(2, at=1:2, lab=c( '1', '2'))
title(main='Fuzzy system prediction on financial forecasting')
title(xlab="Cases")
title(ylab="Volume")
box()
legend(0.0, 1.1, c("Predicted","Actual"), cex=0.8,
       col=c("blue","red"), pch=c(21,21))