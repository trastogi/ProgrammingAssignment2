## makeCacheMatrix and cacheSolve cache the inverse of a matrix.

## makeCacheMatrix takes a matrix and creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) i <<- solve
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of a special "matrix" object.  If the
## inverse has already been calculated (and the matrix has not changed), then the
## cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i    
}
