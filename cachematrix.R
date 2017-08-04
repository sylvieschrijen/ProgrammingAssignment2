## These two functions create an environment in which to cache the inverse of a matrix for later use.

## The first function creates and defines an object in which the inverse matrix can be cached.
## First the variables x and m are initialized for makeCacheMatrix.
## Next the data elements get, set, getmatrix and setmatrix are defined.
## Then a list is made of all the created functions, the items in this list are used in the next function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() { x
  }
  setmatrix <- function(inverted) {
    inverse <<- inverted }
  getmatrix <- function() { 
    inverse }
  list(set = set, 
       get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## The second function is used to actually calculate and/or retrieve the inverse matrix
## First it checks if the inverse is already cached. If so it's retrieved
## Else it calculates the inverse matrix using the list-items created in makeCacheMatrix

cacheSolve <- function(x, ...) {
  inverse <- x$getmatrix()
  if(!is.null(inverse)) {
    message("Retrieving cached matrix")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setmatrix(inverse)
  inverse
}