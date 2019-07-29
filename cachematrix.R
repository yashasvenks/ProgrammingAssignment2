makeCacheMatrix <- function(x = matrix()) {
  l <- NULL
  set <- function(y) {
    x <<- y
    l <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) l <<- inverse
  getinverse <- function() l
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
  l <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(l)
  }
  data <- x$get()
  l <- solve(data, ...)
  x$setinverse(l)
  l
}