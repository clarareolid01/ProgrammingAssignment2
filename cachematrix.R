## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## We create a new matrix in order to be able to invert it.

makeCacheMatrix <- function(x = matrix()) {
        val <- NULL
        ## Set the matrix
        set <- function(y) {
                x <<- y
                val <<- NULL
        }
        ## Get the matrix
        get <- function() x
        ## Set the inverse
        set_inverse <- function(inv) val <<- inv
        ## Get the inverse
        get_inverse <- function() val
        
        list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function
## This function computes the inverse of the matrix created from the previous function. If we have that
## the inverse has already been computed and it has not changed, then the function should retrieve
## the inverse from the previous function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_mat <- x$get_inverse()
        
        ## Check if we have computed the inverse
        if(!is.null(inverse_mat){
                message("Already computed")
                return(inverse_mat)
        }
        
        mat <- x$get()
        inverse <- solve(mat,...)
        x$set_inverse(inverse)
        inverse
}
