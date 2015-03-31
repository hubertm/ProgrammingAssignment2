## 2 functions:
## The first function makeCacheMatrix generates a special "matrix" object
## The second takes the special "matrix" object and, if no inverse matrix is
## stored there, calculates the inverse matrix and caches the result in the
## special "matrix" object and ultimately returns the inverse matrix
## HM 18/3/2015


## Takes a matrix as argument
## Posses 4 function that can be called in order to acces or modify the stored data
## set(), get(), setinverse() and getinverse()
## Generates a special matrix object

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      ## set the matrix
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      ## return the matrix
      get <- function() x
      ## Set the cache with the inverse matrix
      setinverse <- function(inversematrix) inv <<- inversematrix
      ## return the inverse matrix
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The funtion is called with the special matrix object generated
## by makeCacheMatrix as an argument. It checks if the inverse has already been calculated
## and cached. If it cna retrieve the inversed matrix it will end returning it.
## If not it will get the data stored in the special matrix object calling the  makeCacheMatrix$get() function
## calculate the inverse matrix and write the result using the makeCacheMatrix$setinverse() function of the
## special matrix object. Eventually it will return the inversed matrix

cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
}