## makeCacheMatrix: Function that creates a special "matrix" object that can cache its inverse.
## cacheSolve: Function that computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


## Function that creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(matr = matrix()) {
  inverse <- NULL
  set <- function(a) {
    matr <<- a
    inverse <<- NULL
  }
  get <- function() matr
  setInverse <- function() inverse <<- solve(matr)
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
    message("getts the cached data")
    return(inverse)
  }

  #calculate inverse 
  mat.data = x$get()
  inverse = solve(mat.data, ...)

  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inverse)

  return(inverse)
}
