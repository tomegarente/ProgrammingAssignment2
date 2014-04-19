## These functions allows for the user to create a cached Matrix 
## and provided a special Solve function to return the cached inverse calculation
## instead of re-calculating this every time.

## This function creates the cached matrix and provides some methods for it.
## It takes a matrix as an agrument

makeCacheMatrix <- function(x = matrix(x)) {
  
  #symbol to store inverse
  xInv <- NULL
  set <- function(y) {
    x <<- y
    xInv <<- NULL
  }
  
  get <- function() 
  setInverse <- function(solve) xInv <<- solve
  getInverse <- function() xInv
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function will check if a cached inverse is available
## if it is, that will be returned, alternatively the inverse
## will be calculated and stored in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xInv <- x$getInverse()
  
  ## returns cached solution if available
  if (!is.null(xInv)){
    message("getting cached data")
    return(xInv)
  }
  
  ## cached version not available will now calculate the solution
  data <- x$get()
  xInv <- solve(data)
  x$setInverse(xInv)
  xInv
    
}

