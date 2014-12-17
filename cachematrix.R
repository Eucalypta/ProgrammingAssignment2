## Put comments here that give an overall description of what your
## functions do

##The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse matrix
##get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve= setsolve,
       getsolve = getsolve)
}


## cacheSolve returns the inverse of the cached matrix if it has 
##already been calculated
##if and the inverse of the matrix is it has not been calculated yet


cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}



