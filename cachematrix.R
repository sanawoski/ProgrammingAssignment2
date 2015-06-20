
# These two functions cache the inverse of a matrix


## function intakes a matrix object
## and creates a list with functions to 
## set matrix, get matrix, set cached inverse, or get cached inverse

makeCacheMatrix <- function(x = matrix()) {
  
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cache <<- inverse
  getinverse <- function() cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## sees if matrix is cached and returns cached matrix, otherwise solves for matrix inverse 
## and stores to cache

cacheSolve <- function(x, ...) {
  cache <- x$getinverse()
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  matrix <- x$get()
  cache <- solve(matrix, ...)
  x$setinverse(cache)
  cache
}
