## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function takes in a square matrix and if it is not square matrix,
# it stops running
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  if (nrow(x) != ncol(x)) {
    stop("It is not a square matrix")
  } 
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function(){
    x
  }
  setinverse <- function(solve) {
    inv <<- solve
  }
  getinverse <- function() {
    inv
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
