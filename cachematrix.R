## These functions allow the inverse of a matrix
## to be computed, and then cached for future
## retrieval.

## Creates a "special" matrix object that can cache its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    ## Create list of functions
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    ## Return list of functions attached to matrix
    ## object
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of the "special" matrix 
## object returned by the function 'makeCacheMatrix', 
## and caches it for quick retrieval on future queries. 

cacheSolve <- function(x, ...) {
    
    ## Attempt to retrieve cached inverse of matrix 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## Compute inverse of matrix 'x' since it's 
    ## not cached.
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    
    ## Return inverse of matrix 'x'
    inv
}
