## Solution of Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
## 

## Create an object that contain the matrix and its inverse, 
## and functions for retrieve and set the values

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


## The Function solve the inverse of the matrix, if the values is cached, use it, 
## if not calculate it and save as cached
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
    ## Return a matrix that is the inverse of 'x'
}
