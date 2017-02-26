## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y) {
    x <<- y
    invert <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invert <<- inverse
  getInverse <- function() invert
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above.However,it first checks to see if the mean has 
## already been calculated. If so,it gets the mean from the cache and skips the computation
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache
## via the setmean function.

cacheSolve <- function(x, ...) {
  ## Return the matrix which is inverse of 'x'
  invert <- x$getInverse()
  if (!is.null(invert)) {
    message("getting cached data")
    return(invert)
  }
  mat <- x$get()
  invert <- solve(mat, ...)
  x$setInverse(invert)
  invert
}
