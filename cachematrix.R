# The functions below provide a way to create a special matrix object
# that caches its own inverse and returns the cached result immediately
# when available.

## NAME: makeCacheMatrix 
## INPUT: a regular matrix object
## OUTPUT: a special matrix object with embedded storage and functions
##   to aid caching of inverse of matrix
## BEHAVIOR: makeCacheMatrix creates (using lexical scoping) an object
##   with storage for caching the inverse of the matrix, so that future 
##   operations can return the cached result instead

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list (set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## NAME: cacheSolve
## INPUT: a special matrix object created using the makeCacheMatrix function
## OUTPUT: the inverse of the matrix
## BEHAVIOR: The first time cacheSolve is used on a given object, 
##   it will result in the computation of the inverse 
##   of the matrix, and the result will be cached inside the object.
##   The next time cacheSolve is used on the same object, 
##   the cached result will be returned directly.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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
