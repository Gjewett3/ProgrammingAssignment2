## Assignment: Caching the Inverse of a Matrixless 
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## (there are also alternatives to matrix inversion that we will not discuss
## here). Your assignment is to write a pair of functions that caches the inverse
## of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     set_inv <- function(inverse) inv <<- inverse
     get_inv <- function() inv
     list(set = set,
          get = get,
          set_inv = set_inv,
          get_inv = get_inv)
}

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve should retrieve the
## inverse from the cache.

## For this assignment, assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
     inv <- x$get_inv()
     if (!is.null(inv)) {
          message("Inverse already calculated; getting cached matrix")
          return(inv)
     }
     mtrx <- x$get()
     inv <- solve(mtrx, ...)
     x$set_inv(inv)
     inv
}
