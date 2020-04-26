# Programming assignment 2: My exercise
## This program is an example of R functions that, taking advantage of R scoping rules, 
## are potentially able to cash data to save time computation.

## The last lines of this prograsm include a specific section to verify the proper execution 
## of the developpecd functions and the calculation based on a matrix sample M(nrow=3,ncol=3)


## This function, makeCacheMatrix creates a special "matrix", which is really a list 
## containing three functions to: 
## - get the matrix
## - set the matrix inverse
## - get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  get <- function() x
  setinvm <- function(mat) invm <<- mat
  getinvm <- function() invm
  list(get = get, 
       setinvm = setinvm,
       getinvm = getinvm)
}


## The following function calculates the matrix inverse of the special "matrix" created 
## with the functions included in the list defined by the above function makeCacheMatrix. 
## However, it first checks to see if the matrix inverse has already been calculated. 
## If so, it gets the matrix inverse from the cache and skips the computation. 
## Otherwise, it calculates the matrix inverse from the data stored in makeCacheMatrix
## function environmernt and sets the matrix inverse in the cache via the setinvm function.


cacheSolve <- function(x, ...) {
  invm <- x$getinvm()
  if(!is.null(invm)) {
    message("getting cached matrix inverse")
    return(invm)
  }
  mat <- x$get()
  invm <- solve(mat, ...)
  x$setinvm(invm)
  invm
}

# Verification of proper function execution
# M = example of invertible matrix
M <- matrix(c(1,0,0,1,1,0,1,1,1) , nrow = 3, ncol = 3)
M

# First case of a new inverse matrix calculation 
CM <- makeCacheMatrix(M)
Minv <- cacheSolve(CM)
Minv

# Second case regarding the retrival of a cached matrix
Minv <- cacheSolve(CM)
Minv

# Computation verification: Mat_Ver shall be the identity matrix
Mat_Ver <- M %*% Minv
Mat_Ver


