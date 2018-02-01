## Functions to cache the inverse of a given matrix 
## for computational efficiency.

## makeCacheMatrix creates a matrix object,
## returning a list containing a function to set and get
## the value of the matrix, as well as the values of its
## inverse.

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


## cacheSolve checks if the matrix has had its inverse computed,
## and only solves for the inverse if this has not been done.
## Returns the inverse of the matrix either way.
## NOTE! The supplied matrix is always assumed to be invertable

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
