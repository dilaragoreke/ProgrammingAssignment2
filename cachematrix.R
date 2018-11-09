##  Functions that computes the inverse and cache the inverse of a matrix to ease unnecessary computational cost
## by preventing doing the same calculations again

##  makeCachematrix() creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  
  ## creating list for set and get and returnign to parent environenment, functions can be accessed by $ operator
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
   ## Checking if the inverse has already calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
