## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly. Below shows a pair of functions that creates a special
## "matrix" object that can cache its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## reset inverse to NULL when constructing a new Matrix
        inv <- NULL
        
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <- NULL
        }
        
        ## get the value of the matrix
        get <- function() {x}
        
        ## set the value of the inverse
        set_inv <- function(i) {inv <<- i}
        
        ## get the value of the inverse
        get_inv <- function() {inv}
        
        ## list the functions
        list(set = set, 
             get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then it should retrieve the inverse from 
## the cache.


cacheSolve <- function(x, ...) {
        
        ## get the value of the inverse in cache
        inv <- x$get_inv()
        
        ## if there is a cached value just get it and display 
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## otherwise get the value of matrix m to caculate inverse
        m <- x$get()
        inv <- solve(m)
        
        ## and cache it for further retriveal 
        x$set_inv(inv)
        
        ## display the inverse
        inv
        
}
