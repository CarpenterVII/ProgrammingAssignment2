## This code allows to compute the inverse of the matrix
## and cashe the result. If the matrix doesn't change,
## the inverse will be computed only once at the first request
## for the inverse of the matrix and cached for further requests.

## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
## 1.) set the value of the matrix
## 2.) get the value of the matrix
## 3.) set the value of the inverse of the matrix
## 4.) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
      set <- function(y) {
              x <<- y
              i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function checks if the inverse is cached: if yes - gets the inverse,
## if not - computes the inverse and caches it using our special "matrix"

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
      if(!is.null(i)) {
              message("getting cached data")
              return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
