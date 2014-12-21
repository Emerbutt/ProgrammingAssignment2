## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
##1.) set the value of the matrix
##2.) get the value of the matrix
##3.) set the the inverse matrix
##4.) get the the inverse matrix

## cacheSolve, the second function returns a matrix that is the inverse of 'x'


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setss <- function(ss) m <<- ss
  getss <- function() m
  list(set = set, get = get,
       setss = setss,
       getss = getss)
}

## cacheSolve returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getss()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setss(m)
  m
}

