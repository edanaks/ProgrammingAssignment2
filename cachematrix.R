## ProgrammingAssignment2.R
##
## 
## 
## Creates functions which can cache a matrix inverse
## 
## (1) makeCacheMatrix: creates a "special" matrix object suitable for caching
## inverses
## 
## (2) cacheSolve: retrieves inverse from cache, or updates cache if none found.
##
## 
## 
## Acknowledgements: 
## 
## Content is largely modelled over the functions makeVector and
## cacheMean provided in the original assignment description.
##


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

    ## initialize inverse to NULL
	inverse <- NULL
    
    ## set and get functions for the matrix itself
	set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    
    ## setinverse and getinverse functions
	setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    
    ## return the set, get, setinverse, getinverse functions	
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

    ## if the cache is NULL, then calculate the inverse, update the cache and
	## and then return the inverse
	
	if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    actualmatrix <- x$get()
    i <- solve(actualmatrix, ...)
    x$setinverse(i)
    
    ## return the inverse
	i
}
