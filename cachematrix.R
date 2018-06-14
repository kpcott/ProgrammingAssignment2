## The below functions are a way to find and cache the inverse of a matrix and is optimized for large matrices.
## This file contains two function: makeCacheMatrix and cacheSolve.
## When performinging functions on large data, sometimes they can take a long time.
## It can be better to cache calculations and recall than to recalculate functions on large data.


## makeCacheMatrix takes a matrix and caches it in memory. 4 functions are stored as a list in the output.
## The set function stores the function in memory.
## The get function can be used at anytime to recall the stored matrix.
## The setSolve function is used primarily by cacheSolve to store the inverse.
## the getSolve function may be called at anytime to recall a stored inverse.
## Examples on using this function are at the end of the script.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(matSolve) m <<- matSolve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}

## The cacheSolve function will calculate the inverse matrix if it is not already cached.
## If the inverse is already in memory, it will instead return the stored inverse
##  and send the message "getting cached data"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}

## Below are examples of how the functions can be used together.

# A = matrix(1:4, 2, 2)
# A.inv = makeCacheMatrix(A) # Stored the A matrix in memory.
# A.inv$get()
# cacheSolve(A.inv) # Calculates the inverse matrix or recalls from memory.
# A.inv$get()
# A.inv$getSolve() # Optional way to recall inverse.
