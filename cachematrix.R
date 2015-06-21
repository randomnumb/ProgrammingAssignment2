## These functions can cache the inverse of a matrix 
## rather than computing it repeatedly.

## This function makes a special "matrix" which is a list of functions 
## that can cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
      inv.x <- NULL
      set <- function(y) {
          x <<- y
          inv.x <<- NULL
      }
      get <- function() x
      setinv <-function(inverse.x) inv.x <<- inverse.x
      getinv <-function() inv.x
      list(set = set, get=get, 
           setinv = setinv, 
           getinv=getinv)
}


## This function computes the inverse of the "matrix" from makeCacheMatrix if it is null
## and retireves it from the cache otherwise.

cacheSolve <- function(x, ...) {
        inv.x <- x$getinv()
        if(!is.null(inv.x)) {
                message("getting cached data")
                return(inv.x)
        }
        data <- x$get()
        inv.x <- solve(data, ...)
        x$setinv(inv.x)
        inv.x
}