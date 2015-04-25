## These functions allow the inverse of a matrix
## to be computed, and then cached for future
## retrieval.

## Creates a "special" matrix object that can cache its
## inverse.

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


## Computes the inverse of the "special" matrix 
## object returned by the function 'makeCacheMatrix', 
## and caches it for quick retrieval on future queries. 

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
