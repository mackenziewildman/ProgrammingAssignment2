## The following functions compute the inverse of a matrix
## by caching the the value of the matrix and its inverse.
## When computing a matrix inverse, the functions first check
## whether that matrix inverse has already been computed,
## and if so, returns the already computed inverse. If the
## matrix inverse is not already cached, then the functions
## compute the inverse and also cache the value.

## The function makeCacheMatrix creates a vector of functions.
## These four functions are named and perform the following:
## 1 $set(M) set the value of the matrix, takes matrix as input
## 2 $get() get the value of the matrix
## 3 $setinverse() set the value of the inverse, takes matrix as
##   input
## 4 $getinverse() get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) invx <<- solve
    getinverse <- function() invx
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function cacheSolve returns the inverse of a
## matrix. It first checks to see if the inverse has
## already been computed. If so, it returns the inverse
## from the cache. If not, it computes the matrix
## inverse and stores the value in the cache using the
## setinverse function.
## It requires input argument of the type makeCacheMatrix

cacheSolve <- function(x, ...) {
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
