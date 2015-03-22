## This R script has 2 functions that would help cache inverse of a matrix. This would help if the matrix is large
## for which inverse needs to be calculated.

## makeCacheMatrix function takes a matrix as an input and stores the matrix in a different environment. It also
## sets the inverse of the matrix to null. This function returns a list that contains 4 functions that can be used
## to set/get the matrix and the inverse of the matrix.
## for eg: For getting the Inverse-
##
## y<-makeCacheMatrix(yourMatrix)
## y$getInverse

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setInverse <- function(z) inverse<<-z
      getInverse <- function() inverse
      list(set = set, get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}


## cacheSolve function is the core function that implements the caching mechanism. This function checks whether the
## inverse is already calculated. If the inverse is already calculated then it returns the already calculated inverse
## otherwise it calculates the inverse and caches the inverse. This method needs the list as input that is provided by
## makeCacheMatrix function defined above in this script.

cacheSolve <- function(x, ...) {
      inverse <- x$getInverse()
      if(!is.null(inverse)) {
            message("getting cached inverse")
            return(inverse)
      }
      matrix <- x$get()
      inverse <- solve(matrix, ...)
      x$setInverse(inverse)
      inverse
}
