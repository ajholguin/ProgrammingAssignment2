## These functions create a special "matrix" object, which is actually a list 
## with functions to get & set the matrix value, and functions to get & set the 
## inverse of the matrix. The cacheSolve funtion will return a cached version of
## the inverse if it has already been calculated.
## 
## Example usage:
## 
## matr <- makeCacheMatrix()
## matr$set(matrix(c(3,4,2,5,2,4,5,2,3), nrow=3))
## matr$get()
## 
## cacheSolve(matr)    # --> calculate inverse and return
## cacheSolve(matr)    # --> return cached matrix


## This function creates the special "matrix" object. It returns a list of
## functions, which can be used to manipulate the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # i is the inverse of matrix x
  i <- NULL
  
  # function definitions
  set <- function (y) {
    x <<- y       # set matrix to input value
    i <<- NULL    # set to NULL (inverse must be recalculated)
  }
  get <- function () { x }
  setinv <- function (inv) { i <<- inv }
  getinv <- function () { i }
  
  # return a list of functions (use these to manipulate the matrix)
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function takes a "matrix" created by the makeCacheMatrix function and 
## calculates the inverse, caching and then returning the result. If a cached
## version is already available, then it prints a message and returns the cached
## matrix.

cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  
  # if inverse already exists, then return cached data...
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # if not, then calculate, cache, and return the inverse
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

