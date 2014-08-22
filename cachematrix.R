## These functions compute the inverse of a matrix
## if it has not already been computed. 

## makeCacheMatrix takes as input a matrix, and returns a list
## of functions that can modify and retrieve its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## Used to determine if inverse already set
  set <- function(y) { ## Changes value of x
    x <<- y
    m <<- NULL
  }
  get <- function() x ## Retrieves matrix passed
  setinverse <- function(inverse) m <<- inverse ## Sets inverse to m
  getinverse <- function() m ## Retrieves current inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ## Returns list of functions

}


## cacheSolve takes in the list created by cacheMatrix
## and computes the inverse of the matrix,
## if not already initialized

cacheSolve <- function(x, ...) {
        ## Returns either a matrix that is the inverse of 'x', or NULL
  m <- x$getinverse()
  
        ## If inverse has been set, cacheSolve returns the
        ## already computed inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
        ## Else get the matrix then compute, set, and return its inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
