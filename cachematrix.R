## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    actualmatrix <- x$get()
    i <- solve(actualmatrix, ...)
    x$setinverse(i)
    i
}
