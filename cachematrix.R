## This pair of functions allows caching of a potentially expensive matrix inversion.
##
## The makeCacheMatrix routine creates a special version of a matrix that caches
## the result of a matrix inversion (although there is nothing in the makeCacheMatrix
## routine specific to matrix inversion since the routine using it chooses what to set
## the invMatrix value to). 
## routine makes use ...
##
## The cacheSolve routine returns the inverse of the input matrix. If the matrix has
## not yet been encountered, it will compute the inverse and cache it. On subsequent calls
## with the same input matrix, cacheSolve will simple return the cached version. It only
## caches the result for the most recent input matrix.



makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) invMatrix <<- inv
        getinverse <- function() invMatrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mi <- x$getinverse()
        if (!is.null(mi)) {
                message("getting the cached version of the matrix inverse")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data, ...)
        x$setinverse(mi)
        mi
}
