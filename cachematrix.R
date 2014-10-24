# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than 
# computing it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to 
# write a pair of functions that cache the inverse of a matrix.

## HOW TO USE:
# > z = rbind(c(1, -1/4), c(-1/4, 1))
# > x <- makeCacheMatrix()
# > x$set(z)
# > cacheSolve(x)
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
# > cacheSolve(x)
# getting cached data
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
#


## This function creates a special "matrix" object that can cache its inverse.
 <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.
## PS: This funciton assumes that the matrix supplied is always invertible.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m  
}
