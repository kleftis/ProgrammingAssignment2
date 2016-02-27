## Put comments here that give an overall description of what your
## functions do
## The two fuctions below, cache the inverse of a matrix
##
##             FIRST CALL
## Call makeCacheMatrix(v) where "v" is a square matrix
## then call cacheSolve(v) to compute the inverse of matrix "v
##
##             AFTER FIRST CALL
## set() : sets a new square matrix
## get() : returns the square matrix that was input by set()
## getinverse() : returns the inverse of the square matrix, after
##                calculated by cacheSolve().
##
## makeCacheMatrix() creates a special "matrix" object that can
## cache its inverse. makeCacheMatrix() 
## 

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
       setsolve = setsolve,
       getsolve = getsolve)
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

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

