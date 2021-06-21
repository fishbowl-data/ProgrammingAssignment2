## Put comments here that give an overall description of what your
## functions do
#
# This set of functions will allow the user to run "solve()"
# on a matrix and only perform the calculation if the matrix has
# changed since the last time the solve function was run.
# Otherwises it returns the already solved matrix.

## Write a short comment describing this function
#
# When initially called, it will set the solvedMatrix to NULL
# and then set the passedMatrix to the value passed.  It
# also creates the setters and getters for updating the
# matrix at a later time -- which also resets solvedMatrix
# to NULL if new data is passed/set.

makeCacheMatrix <- function(passedMatrix = matrix()) {
  solvedMatrix <- NULL
  
  set <- function(y) {
    passedMatrix <<- y
    solvedMatrix <<- NULL
  }
  get <- function() {
    passedMatrix
  }
  setCachedMatrix <- function(solveMatrix) {
    solvedMatrix <<- solveMatrix
  }
  getCachedMatrix <- function() {
    solvedMatrix
  }
  
  list(set = set, get = get,
       setCachedMatrix = setCachedMatrix,
       getCachedMatrix = getCachedMatrix)
  
}


## Write a short comment describing this function
#
# This function solves the matrix if it hasn't been solved
# already by checking if solvedMatrix is NULL.  If it has
# been solved already (solvedMatrix is not NULL), it returns
# solvedMatrix.

cacheSolve <- function(passedMatrix, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  solvedMatrix <- passedMatrix$getCachedMatrix()
  
  if(!is.null(solvedMatrix)) {
    message("getting cached data")
    return(solvedMatrix)
  }
  
  data <- passedMatrix$get()
  solvedMatrix <- solve(data, ...)
  passedMatrix$setCachedMatrix(solvedMatrix)
  solvedMatrix
  
}
