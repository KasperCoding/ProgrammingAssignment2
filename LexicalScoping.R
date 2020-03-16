## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
## Creating the function SET
  A <- NULL
  set <- function(y) {
    x <<- y
    A <<- NULL}
## Creating the function GET  
  get  <- function() x
## Creating the function setinverse 
  setinverse <- function(solve) A <<- solve
## Creating the function getinverse
  getinverse <- function() A
}

##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Checking if there is the solution allredy in cache.
## If so return the cached data...  
  B <- x$getmean()
  if(!is.null(B)) {
    message("getting cached data")
    return(B)
  }
## ... else solve the equalation  
  Data <- x$get()
  Bm <- solve(Data, ...)
  x$setinverse(B)
## ... and finally return the inverse matrix  
  B
}