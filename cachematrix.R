## The functions below cache a matrix and then calculate the inverse.
## Function makeCacheMatrix creates a matrix and caches it for quicker processing later.

makeCacheMatrix <- function(x = matrix()) {
    ## Define variable.
    invM <- NULL
    ## Sets value of the vector.
    set <- function(y) {
        x <<- y
        invM <<- NULL
    }
    ## Get value of the vector.
    get <- function() x
    ## Set the value of the inverse.
    setinvM <- function(inverse) invM <<- inverse
    ## Get the value of the inverse.
    getinvM <- function() invM
    ## Lists the values of the special "vector" for caching purposes.
    list(set = set, get = get, setinvM = setinvM, getinvM = getinvM)
}

## The function cacheSolve retrieves the created matrix and then computes the inverse.
cacheSolve <- function(x, ...) {
    ## retrieves cached value from the makeCacheMatrix
    invM <- x$getinvM()
    ## Verifies value was calculated.
    if(!is.null(invM)) {
        message("getting cached data")
        return(invM)
    }
    ## Calculates the inverse if the calculation is not cached.
    data <- x$get()
    invM <- solve(data, ...)
    x$setinvM(invM)
    ## invM is returned as the inverse of x.
    invM

}
