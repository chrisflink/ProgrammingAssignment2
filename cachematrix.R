## Collection of two functions to make a cachable matrix and calculate the 
## inverse of a matrix only once. If the inverse is calculated, the result is
## cached.


## Make a cachable matrix from a normal (and invertable) matrix. The matrix is 
## stored and can be set, and get. The Inverse can also be set and get with methods
## setInverse and getInverse.
makeCacheMatrix <- function(x = matrix()) {
        ## When making a new CacheMatrix, the inversed matrix is NULL.
        im <- NULL
        
        ## Function (or method) to set the Matrix, when updating the matrix, the
        ## (cached) inversed matrix should be reset to NULL.
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        
        ## Function (or method) to get the Matrix.
        get <- function() x
        
        ## Function (or method) to set the inverse of the Matrix.
        setInverse <- function(inv) im <<- inv
        
        ## Function (or method) to get the inverse of the Matrix.
        getInverse <- function() im
        
        ## Return the list with functions.
        list(
                set = set, 
                get = get,
                setInverse = setInverse,
                getInverse = getInverse
        )
}

## Get the inverse of a CacheMatrix (constructed with makeCacheMatrix). The inverse
## is cached and only recalculated when the matrix is changed.
cacheSolve <- function(x, ...) {
        ## Try to get the inverse of the matrix.
        im <- x$getInverse()
        
        ## If matrix is returned, the cached inverse is returned.
        if (!is.null(im)) {
                ## Display a message that cached data is used (debugging purpose).
                message("getting cached data")
                ## Return inversed matrix from cache.
                return(im)
        }
        
        ## If NULL is returned, the inverse is not cached.
        ## get data, calculate and cache inverse.
        data <- x$get()
        im <- solve(data)
        x$setInverse(im)
        
        ## Return a matrix that is the inverse of 'x'
        im
}