## Put comments here that give an overall description of what your
## functions do

## This functions, makeCacheMatrix, creates a special matrix object that  can cache its inverse. 


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




## This function, cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix above. If the inverse has already been calculated then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(z, ...) {
  inv <- z$getinv()
  
  data <- z$get()
  inv <- solve(data, ...)
  z$setinv(inv)
  inv
}


