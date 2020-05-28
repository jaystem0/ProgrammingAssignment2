## Functions that cache the inverse of a matrix
## functions do

## this function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    ## initialize the inverse property
  i <- NULL
  
    ## method to set the matrix
  set <- function(matrix) {
      m <<- matrix
      i <<- NULL
  }
  
    ## method to get the matrix
  get <- function() {
      ## return the matrix
      m
  }
  
      ## method to set the inverse of the matrix
  setinverse <- function(inverse) {
      i <<- inverse
  }
  
      ## method to get the inverse of the matrix
  getinverse <- function() {
      ## return the inverse property
      i
  }
  
      ## return a list of the methods
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## this function computes the inverse of the special matrix returned by makecachematrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        ## return the inverse if its already set
        if( !is.null(m)) {
          message("getting cached data")
          return(m)
        }
        
        ## get the matrix from the object
        data <- x$get()
        
        ## calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
        
        ## set the inverse to the object
        x$setinverse(m)
        
        ## return the matrix
        m
}
