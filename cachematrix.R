## makeCacheMatrix creates an object containing a list of functions. The function takes  
## a matrix as argument and calculates and cache the inverse. cacheSolve takes the 
## output from makeCacheMatrix as argument and returns the inverse matix. It first checks if 
## the inverse has alredy been calculated, if not, it makes the calculation and return
## the result.

## This function creates a list containing a number of different functions in order to
## cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  
  get <- function() x
  solveMat <- function(solve) invMat <<- solve
  getInvMat <- function () invMat
  
  return(list( get = get, solveMat = solveMat, getInvMat = getInvMat))

}


## This function returns the inverse of a matrix. If the inverse has already been 
## calculated it gets the result from the cahce.

cacheSolve <- function(x, ...) {
  invMat <- x$getInvMat()
  
  # Check if inverse matrix is in cache and return if true
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  
  # Calculate the inverse if not in cache
  mat <- x$get()
  invMat <- solve(mat, ...)
  x$solveMat(invMat)
  
  ## Return a matrix that is the inverse of 'x'
  return(invMat)
        
}
