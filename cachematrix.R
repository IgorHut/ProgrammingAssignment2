## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setinv<- function(solve) inv_matrix <<-solve
  getinv <- function() inv_matrix
  list(set=set,get=get,
       setinv=setinv,
       getinv=getinv)
}



# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getinv()
  if (!is.null(inv_matrix)) {
    message("getting cached inverse matrix")
    return(inv_matrix)
  } else {
    inv_matrix <- solve(x$get())
    x$setinv(inv_matrix)
    return(inv_matrix)
  }
}