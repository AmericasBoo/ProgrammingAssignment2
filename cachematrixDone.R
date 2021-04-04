## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## library(MASS) is used for calculating the inverse for non squared as
## well as squared matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function()x
        setinv <- function(inverse)inv <<- inverse
        getinv <- function() {
                inver <- ginv(x)
                inver%*%x
        }
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function
## This one is used to get the cache data

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting the cache data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}

