## The two functions store a matrix and cache its inverse

## The first function creates a "matrix" - list that does the following:
## set: sets the value of the input matrix
## get: returns the value of the input matrix
## setinverse: sets the inverse of the input matrix(to use in the cacheSolve function)
## getinverse: returns the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
}


## The second function takes the matrix created above as an input and computes and caches its inverse. 
##It stores the value in the "inv" variable by calling setinverse. 
##That way, if the inverse has already been computed, 
##the function saves time by returning the cached value
##(the "inv" variable is allocated in the same memory slot for both functions
##which is updated using the superassignment operator)

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_data <- x$get()
  inv <- solve(matrix_data, ...)
  x$setinverse(inv)
  inv
}
