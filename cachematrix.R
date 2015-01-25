## 
## 

## A function creating a special object CacheMatrix
## consisting of four functions which are:
## get - get the value of the object, which is square invertible matrix
## set  set the value to the object
## getinverse - get the inverse matrix if it was calculated, otherwise NULL
## setinverse - set the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(m) {
        x <<- m
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}


## A function calculating the inverse of the object CacheMatrix.
## It checks if the inverse value was calculated. If it was it returns this value.
## Otherwise it calculates the inverse matrix of the argument data, sets it
## the inverse value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("Getting cached data")
        return (inv)
    }
    inv <- solve(x$get(), ...)
    x$setinverse(inv)
    inv
}
