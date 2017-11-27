## Matrix inversion could take time. Instead of repeatedly calculating for 
## the inverse of a matrix, the following functions, makeCacheMatrix and
## cacheSolve, caches the inverse of a given matrix which could potentially 
## save time in certain situations.

## The makeCacheMatrix function creates a special matrix and then caches 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
  set <- function(y) {
          x <<- y
          m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the matrix returned 
## by the above function makeCacheMatrix. If the inverse of the matrix has
## already been computed previously (and the matrix has been left unchanged),
## the cacheSolve will return the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
          message("getting cached data")
          return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


