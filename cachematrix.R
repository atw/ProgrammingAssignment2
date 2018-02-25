## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. Functions below create a special "matrix" object that can
## cache its inverse. These functions assume that the matrix supplied is
## always invertible

## makeCacheMatrix function creates a special "matrix" object. It is a list
## containing 4 functions:
##
## set: sets the value of the matrix
## get: gets the value of the matrix
## setinverse: sets the value of the inverse of the matrix
## getinverse: gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" created
## with the makeCacheMatrix function. If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
