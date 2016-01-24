## The two functions created below
## functions do

## makeCacheMatrix function returns a list of 4 functions,first one for
##setting a new matrix, second one for getting the matrix stored within the function the third one to 
## change or set a new inverse of the stored matrix and the last one to get the stored inverse.
## Also, this uses a special "matrix" object to cache its inverse. Getinverse function will return
## the inverse if the inverse is cached froma previous run of the function else it will a return null value
## which is cached if new matix is set by the setmatrix function or the function is run with a new matrix




makeCacheMatrix <- function(x=matrix,...) {
    inv <- NULL
    setmatrix <- function(y=matrix,...)   { 
        x <<- y
        inv <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv 
    list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
    }

## CacheSOlve function calculates the inverse of a matrix if
## there is no inverse in cache
cacheSolve <- function(x,...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
  }
   message("calculating inverse")
      y <-x$getmatrix()
      inv<- solve(y)
      x$setinverse(inv)
      inv
  ## Return a matrix that is the inverse of 'x'