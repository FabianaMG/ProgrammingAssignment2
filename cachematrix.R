## Put comments here that give an overall description of what your
## functions do

## This function creates an object that acts like a matrix whose 
## inverse can be cached.

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y){
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  SetInverse <- function(inverse) iv <<- inverse
  GetInverse <- function() iv
  list(set = set, get = get,
       SetInverse = SetInverse,
       GetInverse = GetInverse)
}


## This function returns the inverse of the matrix-like object computed
## in the function above. If the inverse has already been calculated, 
## it will bring it from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  iv <- x$GetInverse()
  if(!is.null(iv)){
    message("Getting cached data")
    return(iv)
  }
  mat <- x$get()
  iv <- solve(mat, ...)
  x$SetInverse(iv)
  iv
  }
