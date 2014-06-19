## Coursera: R Programming Class
## Programming Assignment #2
## Caching the Inverse of a Matrix

## Tim Judson
## Last Change: 6/19/2014 


## makeCacheMatrix function creates a list of functions that can be used to cache
## and retrieve from that cache an inverse of a matrix.

makeCacheMatrix <- function(x = numeric()) {
  
  m <- NULL
  set <- function(y) {     ## Sets the matrix 
    x <<- y
    m <<- NULL
  }
  get <- function() x11   ## Gets the matrix  
  setSolution <- function(solve) m <<- solve   ## solves (or inverts) the matrix
  getSolution <- function() m                  ## gets the inverse matrix
  list(set = set, get = get,                   ## returns a list of the functions created in makeVector
       setSolution = setSolution,
       getSolution = getSolution)
}


## cacheSolve function checks to see if a matrix has been solved and cached
##if so, it retrieves the matrix; otherwise it solves the matrix

cacheSolve <- function(x, ...) {
  m <- x$getSolution()
  if(!is.null(m)) {                    ## retrieves the matrix if found in cache
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)    ## solve the matrix if not found in cache
  x$setSolution(m)    ## caches the just solved matrix
  m
}
