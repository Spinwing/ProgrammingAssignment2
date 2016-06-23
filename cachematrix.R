# ProgrammingAssignment2, R Programming, Johns Hopkins University on Coursera
# Author: spinwing
# date created: 2016-06-23
#
# Caching the inverse of a matrix
# -------------------------------
# to reduce computational cost, the inverse of the matrix is cached in the 
# lexical scope
#
# makeCacheMatrix  creates the object capable of caching the computed result
# cacheSolve       computes the inverse of a matrix on an object created by 
#                  makeCacheMatrix
#                  - if the inverse has not been calculated yet, calculates it
#                    and caches the result
#                  - otherwise returns the cached value
#

# makeCacheMatrix
# Args:
#   x: the source matrix for which the inverse matrix should be calculated
# Returns:
#   the caching object
makeCacheMatrix <- function(x = matrix()) {
    invmx <- NULL
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    
    get <- function() x
    setinvmx <- function(mx) invmx <<- mx
    getinvmx <- function() invmx
    list(set = set, get = get, setinvmx = setinvmx, getinvmx = getinvmx)
}

# cacheSolve
# Args:
#   x: the caching matrix object created via makeCacheMatrix
# ...: variable number of arguments to be passed to the solve function
# Returns:
#   a matrix which is the inverse of x
#   if the matrix is not cached, it will calculate it
#   otherwise will return the cached copy
cacheSolve <- function(x, ...) {
    invmx <- x$getinvmx()
    if (!is.null(invmx)) {
        message("getting cached data")
        return(invmx)
    }
    
    data <- x$get()
    invmx <- solve(data, ...)
    x$setinvmx(invmx)
    invmx   
}

# end