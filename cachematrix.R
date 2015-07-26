## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##copying makeVector and cachemean to test it
makeVector <- function(x = numeric()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmean <- function(mean) m <<- mean
      getmean <- function() m
      list(set = set, get = get,
           setmean = setmean,
           getmean = getmean)
}

##output of make vecor is returned to cachemean
cachemean <- function(x, ...) {
      m <- x$getmean()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- mean(data, ...)
      x$setmean(m)
      m
}

##Reads a matrix whose inverse has to be computed
##Create a square matrix which is invertible and pass it as a parameter to this function
##store the return value and pass it as a parameter to cachesolve to compute inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
##solve(x) returns inverse of a square matrix
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)   

}


## Write a short comment describing this function
##subsequent calls to this cache solve will return -cached copy of inverse
##rather than computing
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
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
