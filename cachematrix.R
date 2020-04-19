##we need to write two functions that will cache the inverse of a matrix
#

## creating a matrix object, where cache is inverse of input

makeCacheMatrix <- function(x = matrix()) {
  inverse_x <- NULL
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_x <<- inverse
  getinverse <- function() inverse_x
  list(set = set,
       get = get,
       setinverse = setinverse ,
       getinverse = getinverse)

}


## computes the inverse and returns it, if the inverse was already computed, 
##then the function should return the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_x <- x$getinverse()
  if(!is.null(inverse_x)) {
    message("Getting cached data.")
    return(inverse_x)
  }
  data <- x$get()
  inverse_x <- solve(data)
  x$setinverse(inverse_x)
  inverse_x
}
