## Implementation of cached inverse matrix computation

## Usage example
## m  <- matrix(runif(n=36), 6, 6)  # create a random squared matrix
## mc <- makeCacheMatrix(m)         # create the cached matrix 
## cacheSolve(mc)                   # solve for inverse and save result to cached variable
## cacheSolve(mc)                   # second call with information message


## Creates a special "matrix" object that can cache its inverse. 
## This special "matrix" is containing functions to:
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix inverse
## get the value of the matdix inverse
makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse matrix with NULL
  inverseMatrix <- NULL
  
  # setter function; reset inverse matrix
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  # getter function of matrix
  get <- function() x
  
  # setter inverse matrix
  setinverse <- function(inverse) inverseMatrix <<- inverse
  
  # getter inverse matrix
  getinverse <- function() inverseMatrix
  
  # list functions of makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache, otherwise the 
## inverse is recalculated and cached.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # get inverseMatrix from makeCacheMatrix
  inverseMatrix <- x$getinverse()
  
  # check if inverse matrix is already calculated and cached
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  # otherwise get matrix
  data <- x$get()
  
  # calculate inverse matrix with solve function
  inverseMatrix <- solve(data, ...)
  
  # set result into cached variable in makeCacheMatrix
  x$setinverse(inverseMatrix)
  # return inverseMatrix
  inverseMatrix
}
