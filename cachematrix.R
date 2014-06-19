## This script is intended to cache the inverse of a matrix when it is calculated 
## the first time and to return the cached value rather than recalculate when
## the function cacheSolve is repeatedly called.  

## This code was tested on the following matrix:
## m1 <- matrix(data = c(1,3,3,1,4,3,1,3,4), nrow =3, byrow=TRUE)

## makeCacheMatrix stores the inverse value of a matrix

makeCacheMatrix <- function(x = matrix()) {
    mycache <- NULL
    get <- function() x
    setinv <- function(newinv) mycache <<- newinv
    getinv <- function() mycache
    list(get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## cacheSolve checks to see if there is is any cached value for the matrix inverse.
## If not, cacheSolve calculates the inverse and passes that value to mycache for 
## subsequent retrieval.

cacheSolve <- function(x, ...) {
    mycache <- x$getinv()
    if(!is.null(mycache)) {
        message("getting cached data")
        return(mycache)
    }
    data <- x$get()
    mycache <- solve(data, ...)
    x$setinv(mycache)
    mycache
}
