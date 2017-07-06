##----------------------------------------
## Programming Assignment 2 - Alvaro Ortiz
## Date: 2017.07.06
##----------------------------------------

## makeCacheMatrix() takes a regular matrix as argument.
## Then returns it as a Special Matrix, which is able to store its own inverse 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
        }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve() takes a Special Matrix as an argument.
## checks if it already has its inverse cached, if so, retrieves it, if not, 
## calculates and caches it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
