makeCacheMatrix <- function(x = numeric()) {
        # the cached value or NULL if nothing cached
        # initially nothing is cached so set to NULL
        cache <- NULL
        # store a matrix
        setMatrix <- function(newValue) {
                x <<- newValue
                # new value for matrix, NULL to cache
                cache <<- NULL
        }
        # returns stored matrix
        getMatrix <- function() {
                x
        }
        # cache given argument
        cacheInverse <- function(solve) {
                cache <<- solve
        }
        # get cached value
        getInverse <- function() {
                cache
        }
        # returns a list of functions
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}
# The following function calculates inverse of a special matrix created with makeCacheMatrix
cacheSolve <- function(y, ...) {
        # get cached value
        inverse <- y$getInverse()
        # return cached value if it exists
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # otherwise get the matrix, caclulate the inverse and store it in cache
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        # return inverse
        inverse
}