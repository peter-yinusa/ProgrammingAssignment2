## The following functions create a special matrix object that can cache its inverse
## to avoid repeated and costly computations.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}

# Example Usage
# Create a square invertible matrix
my_matrix <- matrix(c(1, 2, 3, 4), 2, 2)

# Create the special "matrix" object
cache_matrix <- makeCacheMatrix(my_matrix)

# Compute and cache the inverse
inverse_matrix <- cacheSolve(cache_matrix)

# Print the inverse matrix
print(inverse_matrix)

# Retrieve the cached inverse
cached_inverse <- cacheSolve(cache_matrix)
print(cached_inverse)
