## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly 
## Below is a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse_x <- NULL
        set <- function(y) {
                x <<- y
                inverse_x <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverse_x<<-inverse
        getInverse <- function() inverse_x
        list(set=set, get=get, setInverse = setInverse, getInverse=getInverse)

}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Assume the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_x <- x$getInverse()
        if(!is.null(inverse_x)) {
            message("getting cached inverse matrix")
            return(inverse_x)
        }
        data <- x$get()
        inverse_x <- solve(data)
        x$setInverse(inverse_x)
        inverse_x
}
