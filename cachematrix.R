## The two functions below will compute the inverse of a matrix and will retrieve the inverse of a matrix from the cache
## if it has already been computed.

## the makeCacheMatrix function will create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
        }
        get <- function() x
        setInverse <- function(invMatrix) inv <<- invMatrix
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## the cacheSolve function will compute the inverse of the special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
