## The function creates a special "matrix" object that can cache its inverse.
## If the inverse has already been calculated (and the matrix has not changed), 
## then it would retrieve the inverse from the cache.

## Caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##  Return a matrix that is the inverse of 'x'
## However, it first checks to see if the matrix has already 
## been calculated. If so, it gets the matrix from the cache 
## and skips the computation. Otherwise, it calculates the 
## inverse matrix and retrun it

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
