## writing functions to enable lexical scpoing in the parent environment

## create a special matrix object that can cache its reverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## compute the inverse of the given matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getsolve()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  i
}
