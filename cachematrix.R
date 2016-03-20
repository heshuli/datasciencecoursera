## Matrix inversion

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        v <- NULL
        set <- function(y){
              x <<- y
              v <<- NULL
        }
        get <- function() x
        setinversion <- function(inversion)   v <<- inversion
        getinversion <- function()    v
        list(set = set, get = get, setinversion = setinversion, getinversion = getinversion)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
      # Return a matrix that is the inverse of 'x'
      v <- x$getinversion
      if(!is.null(v)){
            message("getting cached data")
            return(v)
      }
      data <- x$get()
      v <- solve(data, ...)
      x <- setinversion(v)
      v
}
