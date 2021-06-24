## makeCacheMatrix: Function that creates a special "matrix" object that can cache its inverse.
## cacheSolve: Function that computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


## Function that creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(matrix = matrix()) {
  inverse <- NULL
  set <- function(a) {
    matrix <<- a
    inverse <<- NULL
  }
  get <- function() matrix
  setInverse <- function() inverse <<- solve(matrix)
  getInverse <- function() inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}



## Function that computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
## parameter x is the  output of==from the function makeCacheMatrixinv()

cacheSolve <- function(x, ...) {
  
  inverse = x$getinv()
  if (!is.null(inverse)){
    return(inverse)
  }

  mat.data = x$get()
  inverse = solve(mat.data, ...)

  x$setinv(inverse)

  return(inverse)
}
