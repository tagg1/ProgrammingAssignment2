
## makeCacheMatrix and cacheSolve are two funcitons that work
## together to hold a matrix and cache the inverse of the matrix
## to prevent multiple costly inversions

## makeCacheMatrix is a function that creates a special object
## consisting of a matrix, and, once inverted, a saved copy
## of the inverted matrix. It is a list with the functions:
## set, get, setinverse, getinverse. If the matrix is re-"set",
## the cache is assumed to be invalid and must be recomputed.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of the
## stored matrix in the "CacheMatrix" object. If the inverse
## has already been calculated, the stored inverse is used.
## Otherwise the inverse is calculated and saved.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
