
rm(list = ls())

dataO = read.csv("shuffledYogurt.csv", header=TRUE)
dataO
size = dim(dataO)
nn = (size[2]-1)/2
nn

dataO[,1:65]=scale(dataO[,1:65])

set.seed(121)
#set.seed(873)
sm=sample(size[1], replace=T)
data=dataO[sm,]

apply(data,2,max)

library("xtable")
cc=cor(data[,1:65],data[,1:65],method="pearson")
print(xtable(cc))

plot(abs(cc[,11]))
abs.cor.values<-abs(cc[3:65,11])
#abs.cor.values<-abs(cc[,11])

plot(abs.cor.values)
axis(2, seq(0, 1, 0.1))
axis(1, seq(0, 65, 1))

plot(abs.cor.values,pch=19,cex=1.6,cex.lab=1.6,cex.axis=1.6,font.axis=1.6,xlab="Variables")
axis(2, seq(0, 1, 0.1),pch=19,cex=1.6,cex.lab=1.6,cex.axis=1.6,font.axis=1.6)

cc[8,11]

########## D E C I O N    T R E E ################
library("tree")

D=data[,3:65]
regular <- tree(a65~.,data=D)
summary(regular)
plot(regular)
text(regular)

########### R A N D O M    F O R E S T ################## 
n=46
#install.packages("randomForest")
library("randomForest")

D$a65 = factor(D$a65)
table(D$a65)

set.seed(12)
intrusion.rf = randomForest(y=D$a65,x=D[,1:(length(D[1,])-1)],ntree=n,importance=TRUE)
plot(intrusion.rf)
#getTree(intrusion.rf, k=40, labelVar=FALSE)

# See which variables were important
intrusionImp <- round(importance(intrusion.rf),2)
varImpPlot(intrusion.rf, type=1)

# Type str(intrusion.rf) to get more properties
str(intrusion.rf)
intrusion.rf$err.rate
intrusion.rf$confusion
#intrusion.rf$forest
#intrusion.rf$predicted

