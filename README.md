# matrixcache
## Put comments here that give an overall description of what your
## functions do

## This function makes a 'matrix' object which chaches the inverse of it

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## This function finds out the inverse of the matrix.
## If the inverse is already caluclated, the cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver <- x$getinverse()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinverse(inver)
  inver
}
