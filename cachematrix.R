## ProgrammingAssignment2.R
##
## Creates functions which can cache a matrix inverse
## 1) makeCacheMatrix: creates a "special" matrix object suitable for caching
## inverses
## 2) cacheSolve: retrieves inverse from cache, or updates cache if none found.


################################################################################

## ===============
## makeCacheMatrix
## ===============
## 
## arguments:
## ----------
## x = matrix()
##
## description:
## ------------
## Takes a matrix and creates a special matrix object which can cache inverse
## matrix.
##
## returns:
## --------
## a list (get, set, getinverse, setinverse) of functions which can
## get/set the matrix and get/set the inverse. 


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




## ==========
## cacheSolve
## ==========
## 
## arguments: 
## ----------
## x, ...
##
## description:
## ------------
## Takes a "matrix" returned by makeCacheMatrix and returns its inverse.
## If the inverse has already been cached (and if the matrix has not changed), 
## then the inverse is retrieved,
## else it is computed afresh and then cached.
##
## returns:
## --------
## Inverse of matrix

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
