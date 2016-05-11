# This pair of functions allows the user to cache the inverse of a matrix so that the
# inverse doesn't have to be recomputed each time the user wants to use the inverse of
# the matrix.

# makeCacheMatrix: Convert a matrix into an augmented matrix that can cache its inverse
#
# Inputs:
#
#   x: matrix to convert
#
# Output:
#
#   The augmented matrix
makeCacheMatrix <- function(x = matrix()) {
    cached_inverse <- NULL
    
    set <- function(y) {
        x <<- y
        cached_inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) cached_inverse <<- solve
    getinverse <- function() cached_inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# cacheSolve: Calculate the inverse of an augmented matrix
#
# Inputs:
#
#   x: matrix to calculate the inverse for
#
#   ... Additional parameters passed to the solve() R function
#
# Output:
#
#   The augmented matrix
cacheSolve <- function(x, ...) {

    # If the inverse is cached, return the cached version.
    # Otherwise, calculate and cache the inverse.
    
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    
    # Return the inverse matrix.
    
    i
}
