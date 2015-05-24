## These functions can be used to cache an inverse of a matrix.

## makeCacheMatrix creates a list containing functions that sets and retrieves
## a matrix and its adherent inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i<<-inv
        getinv <<- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Checks if list that was created with makeCacheMatrix has a calculated inverse.
## If inverse exists: return inverse and skip further computation.
## If no inverse: calculate inverse and store in cache.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
