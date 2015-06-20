## the first function creates a matrix that stores its inverse and the second 
## function computes the inverse if its is not already computed and returns the
## the inverse

## Write a short comment describing this function
## This function creates a "matrix" object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        nv <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) nv <<- inverse
        getinverse <- function() nv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## this function calculates the inverse of the matrix produced by the first
## function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Return a matrix that is the inverse of 'x'
        nv <- x$getinverse()
        if(!is.null(nv)) {
                message("getting cached data")
                return(nv)
        }
        data <- x$get()
        nv <- solve(data, ...)
        x$setinverse(nv)
        nv
}