## This is a pair of function that computed and caches the inverse of
## a matrix. It assumes that the input matrix is invertable

## makeCacheMatrix() accepts matrix input and make an object for 
## cacheSolve(). The get(), setInverse() and getInverse() is called
## not when makeCacheMatrix is called, but when cacheSolve() is
## called

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL  ## reset matrix inverse to Null
  set <- function(y){
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) Inv <<- inverse
  getInverse <- function() Inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve() at first fetches the Inverse from makeCacheMatrix(), if
## the inverse has already been calculated, return the message and the
## value; if not, call get(), then compute the inverse, then call function
## setInverse() and return the computed value

cacheSolve <- function(x, ...) { ## the input x is an object created 
                                 ## by makeCacheMatrix()
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInverse()
  if(!is.null(Inv)){
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data)
  x$setInverse(Inv)
  Inv
}
