## Functions which cache the inverse of matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse property
  i <- NULL
  
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  get <- function() {
    ## Return the matrix
    m
  }
  
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  getInverse <- function() {
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cachesolve retrieves inverse from cache if already calculated
cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  ## returns inverse if it is set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data) %*% data
  
  x$setInverse(m)
  
  m
}


