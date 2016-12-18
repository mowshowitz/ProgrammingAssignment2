## This function accepts and stroes a matrix, clears the inverted matrix cache (if it exists), 
## creates functions to reset the data objects, retrieve the passed matrix, solve for its inverse, and
## retreive that inverse, then creates and returns a list containing these functions 
## to the global environment. Assigning this list to an object will be accessible and processable by
## the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
      im <- NULL
      set <- function(y) {
        x <<- y
        im <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) im <<- solve
      getinverse <- function() im
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function accepts the object created by makeCacheMatrix() and checks the cache location for data.
## If it finds it, it returns a message and the data stored in im. Otherwise, it pulls the matrix object
## from makeCacheMatrix, solves for its inverse, then assigns it to the cache location and prints it.

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
      im <- x$getinverse()
      if(!is.null(im)) {
        message("getting cached data")
        return(im) 
      }
      data <- x$get()
      im <- solve(data, ...)
      x$setinverse(im)
      im
}