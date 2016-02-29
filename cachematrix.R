## These functions implement a cached version of matrix inversion.

## First, create a cacheable matrix.
## For example, below we create a random 5x5 matrix
##    mcm <- makeCacheMatrix(matrix(runif(5*5), ncol=5))

## Then, we can calculate its inverse with:
##    inv <- cacheSolve(mcm)

## The first time this request is made, R's solve() function is called
## calculate the inverse.  The result is cached.  Subsequent calls
## return the cached matrix rather than recalculating.

## Note that you can set a new value for the matrix with something like:
##    mcm$set(matrix(runif(5*5), ncol=5)).
## This clears the cache and will cause a new call to solve() on the next
## cacheSolve() call.

## makeCacheMatrix() creates the cacheable matrix solve object.

makeCacheMatrix <- function(m = matrix()) {
  s <- NULL
  set <- function(y) {
    m <<- y
    s <<- NULL
  }
  get <- function() m
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve() takes a cachable matrix created by makeCacheMatrix()
## and calculates its inverse when unavailable in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  mat <- x$get()
  s <- solve(mat, ...)
  x$setsolve(s)
  s
}
