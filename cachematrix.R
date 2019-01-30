

#' Creates a special "matrix" that is a list, containing functions to:
#' 1. set/get the value of the matrix,
#' 2. set/get the value of its inverse.
#' 
#' @param x A matrix.
#' @return A list.
#' @examples
#' makeCacheMatrix(matrix(rnorm(9), nrow = 3)
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


#' Computes the inverse of the special "matrix" returned by makeCacheMatrix.
#' If the inverse has already been calculated (and the matrix has not changed),
#' then cacheSolve retrieves the inverse from the cache.
#' 
#' @param x A list returned by makeCacheMatrix function.
#' @return A matrix
#' @examples
#' mm <- makeCacheMatrix(matrix(rnorm(9), nrow = 3))
#' cacheSolve(mm)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}


## Tests:
## Make sure to test invertible matrices. e.g.
# m <- matrix(rnorm(9), nrow = 3)
# mm <- makeCacheMatrix(m)
# cacheSolve(mm)
# cacheSolve(mm)  # here the inverse will be retrieved from cache.

## The following matrics are NOT ivertible:
# matrix(1:9, nrow = 3)
# matrix(rep(1,9), nrow = 3)
