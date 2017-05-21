## Demonstration of matrix inverse caching functions
## Developed for wk3 of R Programming course

## Function to create special vector to get/set the matrix and get/set its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## Calculate the inverse of a matrix created by the makeCacheMatrix function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if (! is.null(inv)) {
    message("Returning cached data from cacheSolve")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  
  inv
}
