## Programming Assignment 2 in R Programming Module (Coursera)

## makeCacheMatrix should return the created matrix that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  
  inverseM <- NULL
  set <- function(y) {
    
    x <<- y
    
    # using '<<-'assignes to an object y that is an object in an environment different
    # from the current environment
    
    inverseM <<- NULL
  }
  get = function() xinver
  setinverse = function(inverse) inverseM <<- inverseM 
  getinverse = function() inverseM
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve: computes the inverse of special matrix, 'x', created
## using makeCacheMatrix

cacheSolve <- function(x, ...) {
  
  inverseM = x$getinverse()
  
  # if the inverse has already been calculated
  if (!is.null(inverseM)){
    # retrieves the inverse from the cache 
    message("getting cached data")
    return(inverseM)
  }
  
  # calculates the inverse, if none has been calculated yet
  mat.data = x$get()
  inverseM = solve(mat.data, ...)
  
  x$setinverse(inverseM)
  
  # returns the inverse of the matrix x
  return(inverseM)
  
}

## End of programming assignment