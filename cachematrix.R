## Functions create and cache the inverse of a matrix.

## Creates an object to hold a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Holds the inverse matrix
  i <- NULL
  
  # Sets the original matrix and initializes the inverse to NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # Returns the original matrix
  get <- function() x
  
  # Sets the inverse matrix
  setinverse <- function(inverse) i <<- inverse
  
  # Gets the inverse matrix
  getinverse <- function() i
  
  # Returns a list holding the above functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Gets an inverse matrix from cache or created and caches an inverse matrix.
cacheSolve <- function(x, ...) {
  # Attempt to get a cached inverse from the matrix container object
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached inverse")
    return(i)
  }
  # A cached inverse was not found, make one, cache it and return it.
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
