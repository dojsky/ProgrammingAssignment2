##[Assignment2-Jung-Do]makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##For this assignment, assume that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  ## note that this technique allows use of $ oerrator
  ## to access each function from the list
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
##retrieve the inverse from the cache.
##Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is
##a square invertible matrix, then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
 
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mymat <- x$get()
  inv <- solve(mymat, ...)
  x$setInverse(inv)
  inv
}


############TESTING FUNCTIONS############################
source("cachematrix.R")
 aa <- makeCacheMatrix(matrix(c(1,2,3,1,2,1,1,1,1), 3, 3))
aa$get()
aa$getInverse()
 cacheSolve(aa)
 cacheSolve(aa)
aa$getInverse()
aa$set(matrix(c(2, 1, 2, 1,1,3,1,1,2), 3, 3))
aa$get()
aa$getInverse()
invset<-cacheSolve(aa)
cacheSolve(aa)
I<-aa$get()%*%invset
I
