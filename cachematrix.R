## The aim of the function cacheSolve  is to calculate the 
## inverse of an input matrix.
## For that purpose, it uses the function makeCacheMatrix, which 
## creates an object, and can be used to cache the matrix inverse.
## If the inverse is already cached, cacheSolve return 
## the cached inverse matrix. Otherwise, it computes the inverse and
## caches the inverse.
## Using cached values, computational burdon reduces. 

## Fuction makeCacheMatrix (bellow)creates a matrix object, which
## is a list containing a function to
## 1) set the value of the matrix (set)
## 2) get the value of the matrix (get)
## 3) set the value of the the matrix inverse (setInverse)
## 4) get the value of the the matrix inverse (getInverse)


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve calculates the inverse of the object matrix created 
## by function makeCacheMatrix. However, it first checks if the 
## inverse already already exists. If so, it gets the inverse from the 
## cache and retuns. Otherwise, it calculates the inverse of
## the matrix and sets the value of the matrix inverse in the 
## cache via the setInverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}
