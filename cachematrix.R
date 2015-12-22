## Put comments here that give an overall description of what your
## functions do

## First function creates a special "matrix" object that can cache its inverse
## 1) set the value of a given matrix, 
## 2) get the value of a given matrix, 
## 3) set the  value of the inverse of that matrix, and 
## 4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve #defines a function to give the inverse of a given matrix, which caches the inverse
  getinverse <- function() m # function will return the inverse matrix
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned 
## by `makeCacheMatrix` above. If the inverse has already been 
## calculated (and the matrix has not changed), then
## `cacheSolve` retreives the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m) # returns the cached inverse matrix if x$getinverse() returns a value
  }
  data <-x$get()
  m <- solve(data, ...) #if x$getinverse doesn't return a value, gets the matrix and uses the solve function to give the inverse
  x$setinverse(m) #caches the inverse of the matrix using the setinverse() function
  m #prints the inverse matrix
}
