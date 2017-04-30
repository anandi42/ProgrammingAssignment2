## The following functions of cachematrix.r use R's functions and lexical scoping rules 
## to demonstrate how to save calculation time by creating a special cached object consisting of a matrix
## and the outpt of a function, which in this case will be solve(), a R function that solves for 
## the inverse of the matrix. This type of cached functions saves computation time, especially for
## very large matrices. 

##First function: mackCacheMatrix
#This function will:
#1. Use <<- to set matrix object x and object i
#2. set default NULL values of x and i
makeCacheMatrix <- function(x = matrix()) {
  i <<- NULL
  set <- function (y) {
    x <<- y
    i <<- NULL
  }
#2. create functions get, seti and geti to get matrix, set/get inverse
get <- function() x
seti <- function(inverse) i <<- inverse
geti <- function() i
#3. Put it all in a list
list(set=set, get=get, seti=seti, geti=geti)
}

#Second Function: cachecSolve
#This function uses the output of makeCacheMatrix 

cacheSolve <- function(x, ...) {
#add an if clause to get inverse from cache if available
  i <- x$geti()
  if(!is.null(i)){
    message("I already calculated this! Getting from cache!")
    return(i)
  }
#solve for inverse
  mat <- x$get()
  a <- solve(mat,...)
  x$seti(a)
  return(a)
}

##Example output: 
# x <- matrix(runif(4), ncol=2, nrow=2)
# m <- makeCacheMatrix(x)
# ans <- cacheSolve(m)
# ans
#[,1]        [,2]
#[1,] -3.902778  2.51145565
#[2,]  1.796455 -0.03919019
# cacheSolve(m)
#I already calculated this! Getting from cache!
#  [,1]        [,2]
#[1,] -3.902778  2.51145565
#[2,]  1.796455 -0.03919019

#The first run, solve() is run, but the second time, data is pulled from cache. With a small matrix
#I didn't even notice a difference, but maybe for larger matrices...
#I made a second helper function to see the actual time saved myself

