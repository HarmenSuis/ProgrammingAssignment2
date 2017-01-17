# The function 'makeCacheMatrix' function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(b) {
      a <<- b
      inv <<- NULL
      }
  
      get <- function() a
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set,
      get = get,
      setInverse = setInverse,
      getInverse = getInverse)
}

# The function 'cacheSolve' computes the inverse of the special "matrix" created by the function 'makeCacheMatrix'.
# If the inverse has already been calculated (and the matrix has not changed), 
# then it should retrieve the inverse from the cache by putting it last in the function.

cacheSolve <- function(a, ...) {
  ## Return a matrix that is the inverse of 'a'
  inv <- a$getInverse()
  if (!is.null(inv)) {
    message("you get cached data")
    return(inv)
  }
  mat <- a$get()
  inv <- solve(mat, ...)
  a$setInverse(inv)
  inv
}

# tests
source("scripts/Caching the Inverse of a Matrix.R")
> my_matrix$set(matrix(c(5, 10, 6, 8), 2, 2))
> my_matrix$get()
     [,1] [,2]
[1,]    5    6
[2,]   10    8
> cacheSolve(my_matrix)
     [,1]  [,2]
[1,] -0.4  0.30
[2,]  0.5 -0.25
              
> my_matrix$set(matrix(rnorm(9), 3, 3))
> my_matrix$get()
           [,1]       [,2]       [,3]
[1,]  1.4433241 -1.0580062  0.6168790
[2,] -1.3884127  0.2953237 -1.0424226
[3,] -0.3784096 -0.9728041  0.8461106
> cacheSolve(my_matrix)
           [,1]       [,2]       [,3]
[1,]  0.4106171 -0.1585563 -0.4947151
[2,] -0.8431678 -0.7816079 -0.3482207
[3,] -0.7857787 -0.9695548  0.5602631
