# Lab 10
# Numerical Differentiation

# finds the first derivative values given vectors of x and y values
firstDerivative <- function(xVec, yVec){
  xVec <- as.numeric(unlist(xVec))
  yVec <- as.numeric(unlist(yVec))
  yVec <- yVec * 1000 # convert to meters
  firstDev <- vector(mode = "numeric", length = length(xVec)-2)
  #loop through the values and store the derivative
  i <- 1
  while(i < length(xVec)-2){
    firstDev[i] <- ((yVec[i+2] - yVec[i])/(xVec[i+2] - xVec[i]))
    i <- i + 1
  }
  
  #return values
  return(firstDev)
}

# finds the second derivative values given vectors of x and y values
secondDerivative <- function(xVec, yVec){
  xVec <- as.numeric(unlist(xVec))
  yVec <- as.numeric(unlist(yVec))
  yVec <- yVec * 1000 # convert to meters
  secondDev <- vector(mode = "numeric", length = length(xVec)-2)
  #loop through the values and store the second derivative
  i <- 1
  while(i < length(xVec)-2){
    secondDev[i] <- ((yVec[i+2] - (2*yVec[i+1]) + yVec[i])/((xVec[i+1] - xVec[i])^2))
    i <- i + 1
  }
  
  #return values
  return(secondDev)
}

# set wd and read excel file
setwd("~/Current/Numerical Computing/Week 12 N22-N28")
require("readxl")
rocketData <- data.frame(read_excel("data.xlsx"))

plot(rocketData$t.sec, rocketData$d.km, type="l", lty=1, col="blue", main="Distance vs Time",
     xlab="Time in Seconds", ylab="Distance in Kms")

# call first derivative function
firstDev <- firstDerivative(rocketData$t.sec, rocketData$d.km)
# call second derivative function
secondDev <- secondDerivative(rocketData$t.sec, rocketData$d.km)

# plot velocity
plot(rocketData$t.sec[2:(length(rocketData$t.sec)-1)], firstDev, type="l", lty=1, col="blue", main="Velocity vs Time",
     xlab="Time in Seconds", ylab="Velocits in m/s")

# plot acceleration
plot(rocketData$t.sec[2:(length(rocketData$t.sec)-1)], secondDev, type="l", lty=1, col="blue", main="Acceleration vs Time",
     xlab="Time in Seconds", ylab="Acceleration in m/s^2")

firstDev
secondDev
