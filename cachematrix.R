## Put comments here that give an overall description of what your
## functions do

## The following function calculates the inverse of the special "matrix" 
## created with the function below. It first checks to see if 
## the mean has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the matrix and sets the value of it in the cache 
## via the setsolve function.

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
	  getsolve <- function() m
        list(set = set, get = get,
			setsolve = setsolve,
			getsolve = getsolve)
}


        ## Return a matrix that is the inverse of 'x'

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
