## The combination of both functions below calculate the inverse
## of an inversible matrix and stores this in cache. If the inverse
## for a certain matrix is already cached the inverse is retrieved
## from cache and returned otherwise the inverse is calculated,
## stored in cache and returned.

## makeCacheMatrix creates a special matrix that can cache
## its inverse. 

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

## cacheSolve returns the inverse of a special matrix x that is
## created from a regular inversible matrix using the makeCacheMatrix
## function. If the inverse is already cached a message is displayed
## and the inverse matrix from the cache is retrieved and returned.
## If it is not already cached the inverse is calculated and stored
## in cache.

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
