## Put comments here that give an overall description of what your
## functions do

## This function, makeVector creates a special "matrix", which is really a list containing a function to
## set the matrix
## get the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  MInv <- NULL
  set <- function(y) {
    x <<- y
    MInv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) MInv <<- inv
  getinv <- function() MInv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Return a the cache matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
  MInv <- x$getinv()
  if(!is.null(MInv)) {
    message("getting cached data")
    return(MInv)
  }
  data <- x$get()
  MInv <- solve(data, ...)
  x$setinv(MInv)
  MInv
}

