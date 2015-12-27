## Creates a list of functions and sets the inverse to Null. 
## Once the inverse is calculated by cacheSolve, it can be accessed by getInv
## without redoing the calculation again

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## Actual calculation of the inverse of the matrix

cacheSolve <- function(x, ...) { 
  ## Return a matrix that is the inverse of 'x' 
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m
}
