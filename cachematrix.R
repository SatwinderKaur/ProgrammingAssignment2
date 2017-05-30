## Following are a pair of functions that are used to create an object that 
## stores a matrix,calculate inverse(if not already cached) and 
## caches its inverse

## This function will have a matrix as a input,cache its inverse and exposes
## an object with 4 functions to get or set matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function will calculate inverse of the matrix created by 
## makeCacheMatrix function .If the inverse has already been 
## calculated it will retreive the inverse from cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
