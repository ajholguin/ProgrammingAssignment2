## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  # i is the inverse of matrix x
  i <- NULL
  
  set <- function (y) { x <<- y; i <<- NULL } # i is set to NULL (must be recalculated)
  get <- function () { x }
  setinv <- function (inv) { i <<- inv }
  getinv <- function () { i }
  
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

# create matrix
matr <- makeCacheMatrix()

# test accessor and mutator functions
matr$set(matrix(c(3,4,2,5,2,4,5,2,3), nrow=3))
matr$get()

# first call to cacheSolve will calculate, cache, and return the inverse
cacheSolve(matr)

# additional calls will return the cached value...
cacheSolve(matr)

# ... unless the vector has changed
matr$set(matrix(c(3,4,7,5,2,2,5,2,3), nrow=3))

cacheSolve(matr)  # recalculate
cacheSolve(matr)  # return cached value

