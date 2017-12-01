makeCacheMatrix <- function(z = matrix()) {
  inv <- NULL
  set <- function(y) {
    z <<- y
    inv <<- NULL
  }
  get <- function() z
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}		  
cacheSolve <- function(z, ...) {
  inv <- z$getinv()
  
  data <- z$get()
  inv <- solve(data, ...)
  z$setinv(inv)
  inv
}

