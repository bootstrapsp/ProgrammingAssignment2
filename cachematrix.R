## This function creates a special "matrix" object that can cache its inverse
## functions do
## creates a matrix, get and set the inverse in the cache

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv_x <<-inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
## The first if block checks the availability of the inverse of Matrix
## If its not available in, 2nd if block calculates it and 
## sets the output to inv_x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'  
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x)
  } else {
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }
}
