## Ian's functions that invert a matrix and make sure
## the inverse is not being recomputed unnecessarily
## by caching the inverse in the environment


## This function creates a list of functions to set up a 
## matrix cache in the environment

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     
     ##set sub-function
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     
     ##get sub-function
     get <- function() x
     
     ##setinverse sub-function
     setinverse <- function(inverse) i <<- inverse
     
     ##getinverse sub-function
     getinverse <- function() i
     
     ##return a list of these functions
     list(set = set, get = get, setinverse = setinverse, 
          getinverse = getinverse)

}


## This function solves a matrix inverse, but first checks
## if the inverse is cached and uses that if possible
## NOTE that this doesn't check for non-computable inverses

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     
     ##attempt to pull the inverse
     i <- x$getinverse()
     if(!is.null(i)){
          message("getting cached inverse")
          return(i) ##break out
     }
     
     ##if not cached...
     data <- x$get() #pull the data
     i <- solve(data) #run an inverse
     x$setinverse(i) #cache the inverse in case of future use
     i #return the inverse
}
