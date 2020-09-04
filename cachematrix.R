## This function creates a special "matrix" object can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL    # m is the inverse matrix
  # set the value of the matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the inverse
  setinverse <- function(inverse)  m <<- inverse
  # get the inverse matrix
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached inverse matrix")
    return(m)
  }
  # calculates the inverse of the data and sets the
  # inverse matrix in the cache via the setinverse function
  input <- x$get()
  m <- solve(input, ...)
  x$setinverse(m)
  m
}
