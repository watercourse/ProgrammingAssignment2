## The two functions below allow you to create a matrix and to calculate its 
## inverse (not checking if it is invertible) and cache it. If you need the 
## inverse calculation and is already in cache, does not calculate it again.
## This saves time since calculating inverse of large matrixes can be a
## computationally expensive operation.

## Creates a list of four functions to set and get a matrix and to get and set 
## the inverse of the matrix. x stores the values of the matrix and m stores the
## value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Calculates the inverse of a matrix if it hasn't been calculated yet and
## stores it in the cache, otherwise returns its value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}