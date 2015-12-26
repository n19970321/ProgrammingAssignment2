## There are two functions in this file that can cache and solve the inverse of a matrix respectively.

## The first function creates a special "matrix" which cache the matrix inversion.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrixinversion <- function(solve) m <<- solve
  getmatrixinversion <- function() m
  list(set = set, get = get,
       setmatrixinversion = setmatrixinversion,
       getmatrixinversion = getmatrixinversion)
}


## The second function calculates the inverse of the matrix given by the first function.
## Furthermore, it will just return the inversion exsisted if it has been calculated before.

cacheSolve <- function(x, ...) {
  m <- x$getmatrixinversion()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrixinversion(m)
  m
}

## The following are some examples that show how these two functions work:

## Example 1
## > A <- diag(2, 2)
## > A
##      [,1] [,2]
## [1,]    2    0
## [2,]    0    2
## > r1 <- makeCacheMatrix(A)
## > r2 <- cacheSolve(r1)
## > cacheSolve(r1)
## getting cached data
##      [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5

## Example 2
## > B <- matrix(1:4, 2, 2)
## > B
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > r3 <- makeCacheMatrix(B)
## > r4 <- cacheSolve(r3)
## > cacheSolve(r3)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5