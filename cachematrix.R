## Put comments here that give an overall description of what your
## functions do

## Create a list that manages a matrix like a object. Return the list with all the functions needed.

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y) {
              x <<- y
              inv <<- NULL
          }
          get <- function() x
          setinv <- function(inverse) inv <<- inverse
          getinv <- function() inv
          list(set = set, get = get,
               setinv = setinv,
               getinv = getinv)
}


## cacheSolve returns the inverse  of the matrix of the list created by makeCacheMatrix.
## If the matrix has already been calculated, return the value, if not, execute the solve function and store it
## in the x list.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
            message("getting cached")
            return (inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinv(inv)
        inv
}
