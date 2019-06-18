## 07-8-2019
## Caching the inversion of a matrix
#######################################
## The functions below cache the inversion of a matrix. 
## This is helpful to reduce the computation times associated with
## matrix inversion.

## A function to create a special Matrix object that can cache itself. 
## The function can set the value, get the value, set the inverse and 
## get the inverse of the matrix object.
##
## I tested this code with the following invertible matrix
## > A
## [,1] [,2] [,3]
## [1,]    5    1    0
## [2,]    3   -1    2
## [3,]    4    0   -1
## > cacheSolve(makeCacheMatrix(A))
## [,1]    [,2]   [,3]
## [1,] 0.0625  0.0625  0.125
## [2,] 0.6875 -0.3125 -0.625
## [3,] 0.2500  0.2500 -0.500


makeCacheMatrix <- function(x = matrix()) {
  #initialize
  i <- NULL
  
  #set the value of matrix
  set <- function(y) {
    x <<- y
    i <<- NULL #clear cache
  }
  
  #get the value of matrix
  get <- function() x
  
  #set the inverse of the matrix
  setinverse <- function(inverse) i <<- inverse  
  
  #get the inverse of matrix
  getinverse <- function() i 
  
  #return as a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## A function to compute the inverse of the Matrix object returned by 
## the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  
  #get inverse of x. 
  #x is a special Matrix object created by makeCacheMatrix
  i <- x$getinverse()
  #if inverse exists in the cache
  if(!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  #if the inverse matrix is not in the cache,
  #get the value of the matrix
  matrixx<- x$get()
  #solve for inverse
  i <- solve(matrixx, ...)
  #set inverse value to cache
  x$setinverse(i)
  #return inverse matrix
  i
}
