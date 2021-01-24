# Test
require(MTS)

# Air Quality Data Set
# Source: https://archive.ics.uci.edu/ml/datasets/Air+quality


air <- read.csv("Data/AirQualityUCI.csv", header = TRUE, sep = ";")
air <- air[,c(1,2,3,6,13)]
air$Date <- as.Date(air$Date, format="%d/%m/%Y")
air$Time <- as.numeric(air$Time)-2
air$CO.GT. <- as.numeric(air$CO.GT.)
air$T <- as.numeric(air$T)
air$C6H6.GT. <- as.numeric(air$C6H6.GT.)

n = dim(air)[1]

Y=log(air[,3:5])
Y <- air[,3:5]

rates <- Y[2:n,] - Y[1:(n-1),]
rates <- 100*rates

ccm(rates)

par(mfrow=c(3,3))
plot(air$Date,Y[,1],type="l",xlab="",ylab="Log",main="CO.GT")
plot(air$Date,Y[,2],type="l",xlab="",ylab="Log",main="C6H6.GT")
plot(air$Date,Y[,3],type="l",xlab="",ylab="Log",main="T")
plot(air$Date[2:n],rates[,1],type="l",xlab="",ylab="Rate")
plot(air$Date[2:n],rates[,2],type="l",xlab="",ylab="Rate")
plot(air$Date[2:n],rates[,3],type="l",xlab="",ylab="Rate")
acf(rates[,1],main="")
acf(rates[,2],main="")
acf(rates[,3],main="")

acf(Y[,1])
acf(Y[,2])
acf(Y[,3])

