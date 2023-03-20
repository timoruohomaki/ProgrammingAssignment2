
## This function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
    mCache <- NULL # creating empty cache object for inversed matrix

    setMatrix <- function(y) {
        x <<- y
        mCache <<- NULL
  }
  
    getMatrix <- function() x
    setInverse <- function(inverse) mCache <<- inverse
    getInverse <- function() mCache
    
    list( setMatrix = setMatrix,
          getMatrix = getMatrix, 
          setInverse = setInverse,
          getInverse = getInverse)
  
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {

  mCache <- x$getInverse()
  
  if (!is.null(mCache)) {
    # cache exists, returning it instead
    message("Returning cached value")
    return(mCache)
  }
  
  data <- x$getMatrix()
  
  # Fun fact: matrix is invertible if det(data) != 0
  
  if(det(data) != 0) {
    message("Matrix is invertible")
  }
  
  mCache <- solve(data, ...)
  x$setInverse(mCache)
  
  message("Returning new inversion")
  return(mCache)
  
}

# === UNIT TEST ====

message(" Running unit tests:")

m1 <- matrix(c(12,19,32,42,49,56,65,72,99),3,3)

message("Original matrix:")
m1

i1 <- makeCacheMatrix(m1)
cacheSolve(i1)
cacheSolve(i1)
