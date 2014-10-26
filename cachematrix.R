## The function makeCacheMatrix() 
## creates a special "matrix" object 
## that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {

  # create "inverseMatrix" to store the cached inverse matrix
  inverseMatrix <- NULL
  
  # Set the value of the matrix
  set <- function(y) {
    x <<- y
    inverseMatrix <- NULL
  }
  
  # Get the value of the matrix
  get <- function() x
  
  # Set the value of the inverse matrix 
  setInverse <- function(solve)  inverseMatrix <<- solve
  
  # Get the value of the inverse matrix
  getInverse <- function() inverseMatrix
  
  # Return the list 
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function cacheSolve() 
## computes the inverse of the special "matrix" 
## returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverse()
  
  # If the inverse has been already calculated, return it
  if(!is.null(inverseMatrix)) {
      message ("getting cached data")
      return(inverseMatrix)
  }
  
  # Otherwise, if the inverse has not been calculated, the function will do it
  data <- x$get()
  inverseMatrix <- solve(data)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
