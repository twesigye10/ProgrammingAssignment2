## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  if (nrow(x) != ncol(x)) {
    stop("It is not a square matrix")
  } 
  
  set <- function(y) {
    x <<- y
    inv <<- x
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
