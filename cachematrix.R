## Matrix inverse of square matrix.
## The matrix inverse is cached to prevent repeating time consuming matrix inverse operation.
## Usage:
## 1) Create cached matrix using makeCacheMatrix.
## 2) When inverse of the matrix is reqiured it is returned by cacheSolve.
## 3) To get cached matrix use get()
## 4) To set new cached value use set(matr)
## Example:
## m <- matrix( c(1,2,3,4), nrow=2, ncol=2) # matrix to be cached/inverted
## mCached <- makeCacheMatrix() # create matrix cache
## mCached$set(m) # set cached matrix
## mCached$get() # get (print) cached matrix
## cacheSolve(mCached) %*% m  # get matrix inversion and multiply by orig matrix, unit matrix result
## m %*% cacheSolve(mCached)   # multiply orig matrix by its matrix inversion, unit matrix result
## # The inverse is computed only once, second multiplication uses cached value


## Create matrix cache
## To get cached matrix use get() e.g. cache$get()
## To set new cached value use set(matr) e.g. cache$set(matrix( c(1,2,3,4), nrow=2, ncol=2))
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  
}


## Get matrix inverse
## The inverse is computed only when the cached inverse is not available
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached matrix inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinv(m)
  m
}