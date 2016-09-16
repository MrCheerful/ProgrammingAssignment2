## Put comments here that give an overall description of what your
## functions do

########################################################################
## makeCacheMatrix() creates an pseudo matrix object with a series of ##
## functions attached in a list vector for accessing the matrix.      ##
##--------------------------------------------------------------------##
## cacheSolve() is a function that operates on the pseudo matrix to   ##
## calculate and return the inverse of the matrix, the inverse matrix ##
## is stored so that for subsequent requests for the inverse matrix   ##
## the value stored in the cache is provided.                         ##
##--------------------------------------------------------------------##
## Usage Example:                                                     ##
## >a <- makeCacheMatrix( matrix( c(4,2,7,6), 2, 2) )                 ##
## > cacheSolve(a)                                                    ##
## [,1] [,2]                                                          ##
## [1,]  0.6 -0.7                                                     ##
## [2,] -0.2  0.4                                                     ##
## > cacheSolve(a)                                                    ##
## Retrieving cached data                                             ##
## [,1] [,2]                                                          ##
## [1,]  0.6 -0.7                                                     ##
## [2,] -0.2  0.4                                                     ##
########################################################################

## Use this function to create a matrix object with cached inverse

makeCacheMatrix <- function(x = matrix()) {
      inv_m <- NULL           ## inv_m will be the cached inverse of the matrix x
      set <- function(y) {    ## function set will load a new matrix into the object
            x <<- y
            inv_m <<- NULL    ## need to set the inverse matrix to NULL, till calculated
      }
      get <- function() x     ## function to return stored matrix
      set_inv_m <- function(mi) inv_m <<- mi    ## function to store inverse matrix in this environment
      get_inv_m <- function() inv_m             ## function to retrieve stored inverse matrix
      list(set = set, get = get,   ## return the functions as a list to the object
           set_inv_m = set_inv_m,
           get_inv_m = get_inv_m)
}

## Use this function to solve/retrieve the inverse of a matrix stored using makeCacheMatrix.

cacheSolve <- function(x, ...) {
      mi <- x$get_inv_m()     ## Retrieve value stored in the cached inverse matrix object
      if(!is.null(mi)) {      ## If there is a cached inv.matrix return it.
            message("Retrieving cached data")
            return(mi)
      }
      mo <- x$get()    ##Retrieve original matrix stored
      mi <- solve(mo)  ##Solve mo to get inverse matrix mi
      x$set_inv_m(mi)  ##Store the mi matrix in the cache
      mi               ## Return the matrix that is the inverse of 'x'
}
