## Functions that calculate the inverse of a matrix and cache the results
## to improve performance.

## Caches the inverse of the matrix for reuse. If the inverse matrix has
## already been calculated it will return the value immediately from the 
## cache, otherwise it will calculate it and cache it for later use.
makeCacheMatrix <- function(x = matrix()) {
    cachedvalue <- NULL  
    set <- function(y) {                            ## Initialize matrix and cache.
        x <<- y
        cachedvalue <<- NULL
    }
    get <- function() x
    setinv<- function(inv) cachedvalue <<- inv      ## Store inverse in cache.
    getinv <- function() cachedvalue                ## Retrieve inverse from cache.
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Returns the inverse matrix of x.
cacheSolve <- function(x, ...) {
    ## Attempt to retrieve the inverse from the cache.
    cachedvalue <- x$getinv()
    
    ## Return the cached value if it exists.
    if(!is.null(cachedvalue)) {
        message("getting cached data")
        return(cachedvalue)
    }
    
    ## Otherwise get the matrix.
    data <- x$get()
   
     ## Solve for the inverse. 
    cachedvalue <- solve(data, ...)
    
    ## Store it in the cache.
    x$setinv(cachedvalue)
    
    ## Return the inverse.
    cachedvalue
}
