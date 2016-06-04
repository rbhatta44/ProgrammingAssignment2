## This function creates a matrix that can cache its inverse.
## Caching a matrix is beneficial rather than computing it repeatedly.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
               x <<- y
               inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
            get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}


## This function computes the inverse of matrix created by the above function

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("caching data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
