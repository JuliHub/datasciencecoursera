## Firt function creates a special object that can cache its inverse
## Second function checks if the inverse has been already calculated
## If so it will retrieve the inverse from the cache directly
## If not it computes the inverse returned by the first function

## return a list which contains functions to set and get the matrix
## and set and get the inverse

makeCacheMatrix <- function(x = matrix()) {
inver <- NULL
set <- function(y) {
  x <<- y
  inver <<- NULL
}
get <- function ()x
setinver <- function(inverse)inver <<-inverse
getinver <- function()inver
list(set = set, get = get,
     setinver = setinver, 
     getinver = getinver)
}


## return the inverse of the original matrix 

cacheSolve <- function(x, ...) {
    inver <- x$getinver()
    
    # if inverse has already been computed then get it from cache
    if(!is.null(inver)) {
      message("getting cached data")
      return(inver)
      }
    # if not then calculate the inverse
    data <- x$get()
    inver <- solve(data, ...)
    x$setinver(inver)
    return(inver)
  
}
