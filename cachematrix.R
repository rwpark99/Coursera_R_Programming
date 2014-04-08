## Calculate Inverse of a Matrix using cache.
## If cached Inverse of a Matrix exists in the memory, then display it.
## Or, there is no one, calculate it.
## To do this, use a special function containing 4 sub-functions.

## "makeCacheMatrix": This function contains 4 sub-functions in it.
## Why use this this complicating function? To hold the calculation results in memory without noticing it.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

## Cacalute the the inverse of of a matrix .
## If it was already calculated and cashed,  then the cached results are simply returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmean(m)
        m
}

## Example using sample matrix:
matrix.1 <- c(0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0) 
matrix.1 <- matrix(matrix.1, ncol=5, byrow=TRUE) 
fx.inverse <- makeCacheMatrix(matrix.1)
matrix.1
cacheSolve(fx.inverse)
matrix.1%*%cacheSolve(fx.inverse)