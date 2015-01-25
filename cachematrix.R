## Two functions exist below that allow the user to calculate the inverse of a matrix.
## Matrix inversion can sometimes be costly in terms of computation, so the inverse
## can be retrieved from cache if a value exists.

## The makeCacheMatrix function takes an argument x or defaults to an empty matrix and
## sets and gets the value of the matrix and inverse of the matrix.

## The cacheSolve function computes the inverse of a matrix.
## It is assumed that the matrix is invertible.
## If the inverse has already been calculated,
## then the function retrieves the inverse from the cache.
## If no inverse exists in the cache, the inverse is calculated.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ## Set the inverse to NULL to start
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

cacheSolve <- function(x, ...) {
        ## retrieve the inverse for matrix 'x' from cache if it is not NULL
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Cached data exists. Retrieving...")
                return(i)
        }
        ## otherwise solve for the inverse of the matrix
        else {
                message("No inverse calculation found in cache. Calculating inverse of the matrix...")
                data <- x$get()
                i <- solve(data, ...)
                x$setinverse(i)
                i
        }
}
