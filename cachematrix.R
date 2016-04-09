## Programming Assignment 2: Lexical Scoping
## Use lexical scoping to preserve the state of a matrix object
## Cache the inverse of a matrix


# Creates a matrix object that can hold its inverse
makeCacheMatrix <- function(x = matrix()) {
    cachedsolve <- NULL
    set <- function(y) {
        x <<- y
        cachedsolve <<- NULL
    }
    get <- function() {
        x
    }
    setsolve <- function(y) {
        cachedsolve <<- y
    }
    getsolve <- function() {
        cachedsolve
    }
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


# Returns the cached inverse if available
# Otherwise computes then returns the inverse
cacheSolve <- function(x, ...) {
    solveval <- x$getsolve()
    if (!is.null(solveval)) {
        message("getting cached data")
        return(solveval)
    }
    data <- x$get()
    solveval <- solve(data, ...)
    x$setsolve(solveval)
    solveval
}
