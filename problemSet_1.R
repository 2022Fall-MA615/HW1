myName <- 'YalongWang'
#question1
v1 <- c(1:20)
v1
v2 <- c(20:1)
v2
v3 <- seq(1,19,2)
v3
v4 <- rep(c(3,7,11),times=10)
v4
v5 <- rep(c(3,7,11),times=11,length.out=31)
v5

#question2
item1 <- seq(3,6,0.1)
x1 <- exp(item1)*sin(item1)
x1

#question3
item2 <- seq(10,100)
sum1 <- sum(c(item2^3 + 4*item2^2))
sum1

#question4a
label <- rep('label',times=30)
item3 <- c(1:30)
str1 <- paste(label,item3)
str1

#question4b
function1 <- rep('function',times=30)
item4 <- c(1:30)
str2 <- paste(function1,item4,sep = '')
str2

#question5
vs <- paste(c('1','function','NA',seq(1,5,2),0.125),collapse = ',')
vs

#question6
A <- matrix(c(1:9),3,3)
m1_ans <- A%*%A%*%A

#question7
row1 <- rep(12,times=17)
row2 <- rep(-12,times=17)
B <- matrix(c(row1,row2,row1),17,3)
m2_ans <- t(B)%*%B
m2_ans
#question8
c2 <- c()
for(i in 1:5){
  ab <- -1
  c1 <- c()
  t <- i
  for(a in 1:5){
    c1 <- c(c1,t)
    if(t == 1){
       ab <- 1
    }
    t <- t + ab
  }
  c2 <- c(c2,c1)
}
A1 <- matrix(c2,5,5)
A2 <- matrix(c(7,-1,-3,5,17),5,1)
m3_ans <- solve(A1,A2)
m3_ans
#question9a
xv <- seq(0,1,0.1)
function1 <- function(xv){
  result <- c()
  for (i in 1:length(xv)){
     result <- c(result,xv[i]^i)
  }
  result
}
func1_ans <- function1(xv)
func1_ans

#question9b
function2 <- function(vx){
  result <- c()
  for (i in 1:length(xv))
    result <- c(result,(xv[i]^i)/i)
  result
}
func2_ans <- function2(xv)

#question9c
function3 <- function(x,n){
  1 + sum((x^(1:n)/(1:n)))
  }
  
func3_ans <- function3(xv,length(xv))
#question10
cel_to_far <- function(t){
  result <- 32 + t * 1.8
  result
}
far_to_cel <- function(t){
  result <- (t -32)/1.8
  result
}

#question11
function4 <- function(x){
  result <- c()
  for(i in x){
    if(i %% 2 != 0){
         result <- c(result,i)
    }
  }
  result
}
odd_ans <- function4(c(1:2000))

#question12
function5 <- function(r){
  result <- 0
  for(i in 1:r){
    result <- result + i^0.5/(11 +3.5*r^1.2)
  }
  result
}
sum_ans <- sapply(10,function5)
sum_ans
#question13
modNumber <- function(x,y){
  result <- x
  while(result %% y != 0){
    result <- result + 1
  }
  result
}
#question14
numberOfWheels <- function(x){
  result <- switch (x,
    unicycle = 1,
    bike = 2,
    car = 4,
    truck = 4,
    tricycle = 3,
    motorcycle = 2
  )
}

#question15
myFactorial <- function(x){
  result <- 1
  while(x != 0){
    result <- result * x
    x <- x-1
  }
  result
}

#question16
myCustomFactorial <- function(x,y){
  if(abs(x-y)>1){
    result <- factorial(max(x,y))/factorial(min(x,y))
    result
    }
  else{
    0
  }
}
#question17
library(datasets)
data("rivers")
customRiverMean <- function(maximum){
  y <- (rivers[rivers < maximum])
  output <- mean(y)
  output
}
#question18
library(datasets)
data("ToothGrowth")
longTeeth <- c()
for(i in ToothGrowth$len){
  if(i >= 15){
    longTeeth <- c(longTeeth,i)
  }
}
longTeeth
#question19
library(datasets)
data("mtcars")
result <- apply(mtcars,2,mean)
averageHorsePower <- result[['hp']]
averageHorsePower
averageWeight <- result[['wt']]
averageWeight

#question20
function6 <- function(x){
  count <- 0
  for(i in 2:length(x)){
    if(x[1] > x[i]){
      count <- count +1
    }
  }
  count
}
function7 <- function(x,y){
  all <- list()
  t <- 1
  for(i in x){
    item <- c(i,y)
    print(item)
    all[t] <- list(item)
    t <- t + 1
  }
  print(all)
  z <- sapply(all,function6)
  z
}
