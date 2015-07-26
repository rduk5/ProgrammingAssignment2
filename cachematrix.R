## These two functions create a special matrix that can cache its inverse
## Then computes the inverse of a matrix, if the inverse is already available in the cache then the function
## returns the cached values. Otherwise the function computes the inverse from scratch

## This function makeCacheMatrix creates a vector/matrix which has the following properties
## sets the value of the matrix/vector
## gets the value of the matrix/vector
## sets the value of the Inverse of the matrix
## gets the value of the Inverse of the matrix

makeCacheMatrix<- function(x=numeric()){
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get <-function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set=set, get=get,setSolve=setSolve,getSolve=getSolve)
}

## This function returns the inverse of a matrix, either from the cache (if it has already been calculated)
## or from scratch by calling on the prioir function makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}