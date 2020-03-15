## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
## Creating the inverse Matrix y...
  y <- solve(x)
## ... and cache the Matrix x and it's inverse Matrix y in z
  z <<- list(x,y)
## ... and finally returns the inverse Matrix
  y
}

##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Checking if there is the solution allredy in cache.
## If so return the cached data...  
  Z <- z
  if(identical(x,Z[[1]])) {
    message("getting cached data")
    y <-Z[[2]]
    return(y)
  }
## ... else solve the equalation  
  else {y <- solve(x)
  return(y)}
## ... and finally return the inverse matrix  
  y
}