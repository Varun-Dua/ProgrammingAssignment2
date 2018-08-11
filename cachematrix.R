## Create a special Matrix and compute the inverse of the Matrix. If the inverse is already computed, fetch the cached inverse.

## Function to create a special Matrix, that can cache its inverse.

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


## Computes the inverse of the special Matrix, assuming it is invertible. If already calculated (for the same Matrix) it fetches the cached inverse.

cacheSolve <- function(x, ...) {
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
