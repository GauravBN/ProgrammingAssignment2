
## These functions have been written for R programming Assignment Week 3
## GitHub user: GauravBN Date: 21st July 2020

## The below function creates a special matrix object that can cache its inverse

  
  makeCacheMatrix <- function(x = matrix()) { ## x is defined as a matrix variable
    Inv <- NULL                             ## Inv will hold matrix inverse, NULL initially 
    set <- function(y) {                   
      x <<- y                             ## value of matrix in parent environment
      Inv <<- NULL                        ## if there is a new matrix, reset inv to NULL otherwise it will continue as is
    }
    get <- function() x                     ## define the get fucntion - returns value of the matrix argument
  
    setinverse <- function(inverse) Inv <<- inverse  ## assigns value of Inv in parent environment
    getinverse <- function() Inv                     ## gets the value of Inv 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)   
  }
  
  
  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
  ## If the inverse has already been calculated before and the matrix supplied has not changed,
  ## then cacheSolve will just return the inverse computed before. If not, then 
  ##cacheSolve will compute the inverse using solve() function
  
  cacheSolve <- function(x, ...) {
    Inv <- x$getinverse()
    if(!is.null(Inv)) {
      message("getting cached data")
      return(Inv)
    }
    data <- x$get()
    Inv <- solve(data, ...)
    x$setinverse(Inv)
    Inv
  }