## This script contains two functions that are used to create a special matrix object and cache 
## the inverse of the matrix


## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ## Assigns the argument to the x and assigns NULL to inv variable
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    ## value of x is retrieved from parent environment
    get <- function() x
    
    ## Assigns input argument to inv
    setinverse <- function(solve) inv <<- solve
    
    ## value of inv is retrieved from parent environment
    getinverse <- function() inv
    
    ## Returns an object, makeCacheMatrix() that contains four functions set(), get(), setinverse(), getinverse()
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}



## This function computes the inverse of the matrix returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then the cachesolve retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## retrives the inverse from the object passed as an argument
  
  inv <- x$getinverse()
  
  ##checks to see whether the result is NULL. Since previous function sets the cached inverse to NULL whenever 
  ## a new matrix is set, if the value is not equal to NULL, cached inverse is return to the parent environment
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## if the output of above is false, cachesolve() gets the mattrix, calculates its inverse
  
  data <- x$get()
  
  inv <- solve(data, ...)
  
  ## setinverse() is used to set the inverse and inverse is returned to the parent environment
  
  x$setinverse(inv)
  
  inv
}
