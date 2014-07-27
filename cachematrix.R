## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  # create "invMat" to store the cached inverse matrix
  inverseMatrix <- NULL
  
  # Set for the matrix
  set <- function(y) {
    x <<- y
    inverseMatrix <- NULL
  }
  
  # Get for the matrix
  get <- function() x
  
  # Set for the inverse matrix 
  setInverse <- function(solve)  inverseMatrix <<- solve
  
  # Get for the inverse matrix
  getInverse <- function() inverseMatrix
  
  # Return the list 
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

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
