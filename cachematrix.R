## There are two function here makeCacheMatrix() 
## and cacheSolve


##  This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
  a <- NULL
  set <- function(b) {
    m <<- b
    a <<- NULL
  }
  get <- function() m
  setmatrix <- function(solve) a <<- solve
  getmatrix <- function() a
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
  
}


## This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(m, ...) {
  ## Return a matrix that is the inverse of 'x'
  a <- m$getmatrix()
  if(!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  data <- m$get()
  a<-  solve(data, ...)
  a$setmatrix(a)
  a
}
