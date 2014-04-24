## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The following function creates a special "matrix"
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  # set the stored inverse value to NULL
  mInv <- NULL
  # set value of the matrix
  set <- function(y) {
    x <<- y
    mInv <<- NULL   # since the matrix changed
  }
  # get the value of the matrix
  get <- function() x
  # set the inverse
  setMatInv <- function(matInv) mInv <<- matInv
  # get the inverse
  getMatInv <- function() mInv
  # return a list of functions
  list(set = set, get = get,
       setMatInv = setMatInv,
       getMatInv = getMatInv)    
}

## Write a short comment describing this function
## The following function calculates the inverse of the special 
## "matrix".

cacheSolve <- function(x, ...) {
  # check if the inverse is already cached
  mInv <- x$getMatInv()
  if(!is.null(mInv)) {
    message("getting cached data")
    return(mInv)
  }
  # get the matrix into data if not cached
  data <- x$get()
  # compute the inverse
  mInv <- solve(data, ...)
  # cache the inverse
  x$setMatInv(mInv)
  # return it
  mInv
}
