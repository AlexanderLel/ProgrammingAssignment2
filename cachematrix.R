## Put comments here that give an overall description of what your
## functions do

# Matrix inversion is usually a costly computation and
# there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.

# Below are two functions that are used to create a special object "Matrix" to cache the inverse of a matrix.


## Write a short comment describing this function

# The function "makeCacheMatrix" creates a special "Matrix" object that can cache its inverse.
# The function "makeCacheMatrix" creates a list containing a function to

# 1. set the value of the matrix.
# 2. get the value of the matrix.
# 3. set the value of inverse of the matrix.
# 4. get the value of inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <- NULL
      }
      get <- function() x
      setInverse <- function(inverse)
            inv <<- inverse
      getInverse <- function() inv
      list(set = set, 
           get = get, 
           setInverse = setInverse, 
           getInverse = getInverse)
}


## Write a short comment describing this function

# The function below returns the inverse of the matrix.
# 1. Step: checks if the inverse has already been computed.
# If so (2. Step), the result will be returned and the computation will be skiped.
# If not (3. Step), it computes the inverse and sets the value in the cache via setInverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      matrix_data <- x$get()
      ## Return a matrix that is the inverse of 'x'
      inv <- solve(matrix_data, ...)
      x$setInverse(inv)
      inv
}
