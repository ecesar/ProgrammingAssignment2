## Author: Eduardo César
## Created: 15/11/2014
## Last modified: 15/11/2014
## This file contains two funcions:
##    makeCacheMatrix, which creates a special matrix object able to cache its 
##                     inverse
##    cacheSolve, which returns the cached inverse of a special matrix if it has
##                been previously calculated and the matrix has not changed, or 
##                calculates, caches and returns the inverse of the matrix if not

## Function: makeCacheMatrix
## Author: Eduardo César
## Created: 15/11/2014
## Last modified: 15/12/2014
## Parameters: x data contents (matrix) of the special matrix object
## Short description: this function creates a special matrix object able to 
##                    cache its inverse
## Detailed Description: this function creates an object (in the OO sense of 
##                       the word), which includes a matrix (the data) and 
##                       several methods for managing this data (set and get)
##                       and for managing and caching the inverse of the 
##                       matrix (setinv, getinv). All these methods are packed
##                       in a list for them to be accessed.
## Returns: an special matrix object

makeCacheMatrix <- function(x = matrix()) {
            inverse <- NULL
            set <- function(y) {
                  x <<- y
                  inverse <<- NULL
            }
            get <- function() x
            setinv <- function(inv) inverse <<- inv
            getinv <- function() inverse
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
}


## Function: cacheSolve
## Author: Eduardo César
## Created: 15/11/2014
## Last modified: 15/12/2014
## Parameters: x special matrix object
## Short description: this function returns the cached inverse of a special 
##                matrix if it has been previously calculated and the matrix 
##                has not changed, or calculates, caches and returns the 
##                inverse of the matrix if not
## Detailed Description: this function gets the cached value of the special 
##                      matrix object x, if this value is NULL it means that
##                      either the matrix has changed or its inverse has 
##                      never been calculated. In this case the inverse of the 
##                      matrix stored in the special matrix object is calculated
##                      using the solve function (the matrix must be invertible),
##                      the inverse is cached in the special matrix object and
##                      returned to the user.
##                      If not then the inverse has been calculated in the past
##                      and the function returns the cached value.
## Returns: the inverse of the matrix of the special matrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse <- x$getinv()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinv(inverse)
      inverse
}
