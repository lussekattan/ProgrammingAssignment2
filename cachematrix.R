## Caching the inverse of a matrix
## The following two functions, makeCacheMatrix and cacheSolve are used to 
## create an object that contains a list with functions for setting and 
## retreiving the matrix and the value of its inverse. As long as the inverse has
## never been calculated using cacheSolve the inverse is set to null, but in case 
## the inverse has been calculated the value is stored in the object and as long 
## as the value of the matrix does not change the cached inverse is returned.


## makeCacheMatrix takes a matrix and returns a list with functions:
## get() returns the matrix
## set() changes the value of the matrix and sets the inverse to null
## getinverse() returns the value if the inverse if it has been caluclated, else NULL
## setinverse() sets the value of the inverse and is used by the function cacheSolve
##
## By using makeCacheMatrix the time consuming computations of the inverse of a 
## matrix can be saved by retreiving the cached value instead of recalculating it.

makeCacheMatrix <- function(x = matrix()) {
    m.inverse <- NULL
    set <- function(y) {
        x <<- y
        m.inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(calc.inv) m.inverse <<- calc.inv
    getinverse <- function() m.inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve checks if the inverse of the matrix has already been solved and stored.
## If it has, it returns the cached value of the matrix. If it has not already 
## been calculated, it calculated the inverse of the matrix and caches the value in
## the object by using the function setinverse() of the object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m.inverse <- x$getinverse()
    if(!is.null(m.inverse)) {
        message("getting cached data")
        return(m.inverse)
    }
    data <- x$get()
    m.inverse <- solve(data, ...)
    x$setinverse(m.inverse)
    m.inverse
}


