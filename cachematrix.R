## My functions create a matrix that is reversible and creates its inverse and caches 
## the inverse of the matrix to the following functions


## This function creates a matrix and its inverse and caches the inverse to the next functions


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y = matrix()) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function returns the inverse of the matrix created in the previous function, x. It caches the
## inverse of x if the previous function has already created it; otherwise, it creates the inverse of
## x and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse() 
        if(!is.null(m)) {
                message("Getting Cached Data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
