## These two functions will look for the matrix if it is solved previously. 
## If it is already solved it returns the cached data
## If it did not solve previously it returns the solved matrix.

## This function takes the matrix x and defines the four functions 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #settting the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #getting the value of the matrix
  get <- function() x
  #setting the inverse of the matrix 
  setinverse <- function(inverse) m <<- inverse
  #getting the inverse of the matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes the input x and looks if there is a cached data which is already solved
## If not it solves the inverse and returns it

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  #to check if the matrix is already solved and cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #if it doesnot exist in cache it solves the inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Returning a matrix that is the inverse of 'x'
}
