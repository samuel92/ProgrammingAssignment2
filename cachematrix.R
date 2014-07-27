## This function, makeCacheMatrix, creates a special "matrix", which is a list containing a function to
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


## This function, CacheSolve, returns the "cache" matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
        MInv <- x$getinv()
        if(!is.null(MInv)) {
                message("getting cached data")
                return(MInv)
        }
        data <- x$get()
        if (dim(data)[1] == dim(data)[2]) {
                MInv <- solve(data, ...)
        }
        x$setinv(MInv)
        MInv
}

