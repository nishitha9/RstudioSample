x <- 10
y <- 15
vectorExample <- c("Hey","There","test")
print(vectorExample[1])
sumOfTheNumbers <- function(x,y){
  y <- x+y
}

listEg <- list(1,c(1,2,3),list(1,2,3323232))
vector <- listEg[2]
print(vector[1])
vector1 <- vector[[1]]
print(vector1[2])