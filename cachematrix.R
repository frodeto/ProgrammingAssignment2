## The following functions enables caching of the inverse of a matrix. One function
## wraps the matrix and its lazily loaded inverse and defines functions for accessing them.
## The other executes the inverse algoritm (solve) and stores the inverse.

## makeCacheMatrix takes a matrix as input argument and defines a set of functions for
## accessing the matrix and its inverse. It returns a list of these functions.
makeCacheMatrix <- function(x = matrix()) {
        x_inverse <- NULL
        
        set <- function(y) {
                x <<- y
                x_inverse <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) x_inverse <<- inverse
        getinverse <- function() x_inverse
        
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes a makeCacheMatrix as input and returns the inverse from the cache if set.
## If the inverse is not already calculated, it calculates the inverse and stores the result
## in the cache.
## The inverse is returned as "invisible" to be able use with large matrices without 
## flooding the console.
cacheSolve <- function(x, ...) {
        x_inverse <- x$getinverse()
        if(!is.null(x_inverse)) {
                message("getting cached data")
                return(invisible(x_inverse))
        }
        data <- x$get()
        x_inverse <- solve(data, ...)
        x$setinverse(x_inverse)
        invisible(x_inverse)
}
