

## These functions create an object to hold a matrix, compute the inverse of the matrix and store it,
## recall the inverse if it has already been stored, set a new matrix or its inverse, and recall the
## matrix or its inverse.

## The makeCacheMatrix function takes a matrix as its argument and creates an object with the ability to set or get
## either the matrix or its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The cacheSolve() function first checks to see if the makeCacheMatrix object has stored the inverse of
## the matrix and returns that inverse. If it has not already stored the inverse of the matrix,
## the function gets the matrix and passes it to the solve function, which computes the inverse of
## the matrix. cacheSolve() then uses setinverse() to store the inverse in the makeCacheMatrix object.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
