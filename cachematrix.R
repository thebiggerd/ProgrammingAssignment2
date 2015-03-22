##This will utilize the solve() to return an inverse matrix.  Since the solve() is very 
## slow, we will return the object from cache if it is available and the matrix hasn't changed.

## This will create the matrix object and setup the cache values.  

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  solve1 = NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve1) m <<- solve1
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function will check the cache and if the matrix has changed and return the inverse of the matrix.
## If the matrix has not changed, this will return what is in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m) && identical(x$setsolve(m), x$getsolve())) {
    message("getting cached data")
  
    return(m)
  }
  data1 <- x$get()
  m <- solve(data1, ...)
  x$setsolve(m)
  m
  
}
