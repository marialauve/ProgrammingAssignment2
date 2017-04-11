## WEEK 3 HW 2
## Program to cache time-consuming computations

## "makeCacheMatrix" creates a special "matrix" object that can cache
## its inverse.
## Note: If X is a square invertible matrix, then solve(X) returns its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinv <- function(solve) s <<- solve
    getinv <- function() s
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}

## "cacheSolve" computes the inverse of the special "matrix" returned
## by makeCacheMatrix above, but first checks to see if it is already
## cached.

cacheSolve <- function(x, ...) {
    s <- x$getinv()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinv(s)
    s
}
