## Jignesh Gor

## makeCacheMatrix function will create a special matrix which will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
  m <- NULL
  
  set <- function(y){
      x <<- y
      m <<- NULL
    }
  
  get <- function() x
  
  setsolve <- function(solve) m <<- solve
  
  getsolve <- function() m
  
  list( set = set,
        get = get,
        setsolve = setsolve,
        getsolve = getsolve)
  
}


## cachesolve function returns the matrix that is the inverse of 'x' from cache if available 
## else generates and caches the inversed matrix for faster retrieval for future request.

cachesolve <- function(x, ...) {
  
  m <- x$getsolve()
  
  if(!is.null(m))
  {
    message("Retrieving cached data...")
    return(m)
  }
  else
  {
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    return(m)
  }
}
