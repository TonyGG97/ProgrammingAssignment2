makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set<- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() x
  list ( set=set, 
         get=get,
         getinverse = getinverse, 
         setinverse = setinverse)
}




cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  newMatrix <- x$get()
  m <- mean(newMatrix , ...)
  x$setinverse(m)
  m
}