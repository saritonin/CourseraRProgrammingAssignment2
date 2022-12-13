## Coursera
## R Programming - Johns Hopkins University
## Week 3
## Programming Assignment 2: Lexical Scoping
## 2022-12-13 SKL Created this file
#-------------------------------------------------------------------------------
## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly (there are also alternatives to matrix inversion that we will 
## not discuss here). Your assignment is to write a pair of functions that 
## cache the inverse of a matrix.

#-------------------------------------------------------------------------------
# makeCacheMatrix
#-------------------------------------------------------------------------------
## This function creates a special "matrix" object that can cache its inverse.
## 
## It is actually a list of sub-functions that set and get the input matrix
## as well as a cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  
   # initialize the inverse storage variable
   inv <- NULL
   
   # set/get the actual input matrix
   set <- function(y = matrix()) {
     x   <<- y
     inv <<- NULL
   }
   get <- function() x
   
   # set/get the inverse matrix
   setinv <- function(calculatedInverse) inv <<- calculatedInverse
   getinv <- function() inv
   
   # define the list
   list(set = set, get = get, setinv = setinv, getinv = getinv)
}

#-------------------------------------------------------------------------------
# cacheSolve
#-------------------------------------------------------------------------------
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.
## 
## Computing the inverse of a square matrix can be done with the solve 
## function in R. For example, if X is a square invertible matrix, then 
## solve(X) returns its inverse.
##
## For this assignment, assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  
  # attempt to retrieve cached inverse
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    inv
  } else {
    # no cache found; continue processing
    inputMatrix <- x$get()
  
    # verify invertibility
    # first, check for squareness
    if (dim(inputMatrix)[1] != dim(inputMatrix)[2]) {
      stop("unable to calculate inverse; matrix is not square")
    }
  
    # second, verify non-zero determinant
    if (det(inputMatrix) == 0) {
      stop("unable to calculate inverse; determinant = 0")
    }
  
    # Return a matrix that is the inverse of 'x'
    calculatedInverse <- solve(inputMatrix)
    x$setinv(calculatedInverse)
    message("calculation has been cached")
    calculatedInverse
  }
}
