## This function enables the caching of the Inverse of a Matrix,
## By which are seperated in two functions.

## This function creates a special matrix object and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set_matrix <- function(y) {
            x <<- y
            invrs <<- NULL
        }
        get_matrix <- function() x
        set_inverse <- function(inverse) invrs <<- inverse
        get_inverse <- function() invrs
        list(set_matrix = set_matrix, get_matrix = get_matrix,
             set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function computes the inverse of a square matrix, 
## Which verifies whether the former function had already computed 
## the inverse, and retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        invrs <- x$get_inverse()
        if(!is.null(invrs)) {
                message("please wait a second while acquiring cached data...")
                return(invrs)
        }
        ## Return a matrix that is the inverse of 'x'
        data <- x$get_matrix()
        invrs <- Solve(data, ...)
        x$set_inverse(invrs)
        invrs
}
