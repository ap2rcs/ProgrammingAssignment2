## Put comments here that give an overall description of what your
## functions do

## Make cacheMatrix creates matrix in different scope and calculates its inverse and assigns it to 
## different scope. Inverse of an matrix is stored as well. It returns a list of functions for 
## get,set matrix object as well as its inverse.

makeCacheMatrix <- function() {
  minv <- NULL
 
## Initialize matrix  
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
##  Retrun matrix stored in environment
  get <- function() {
    x
  } 
## Store inverse of a matrix calculation
  setInverse <- function(inv) {
    minv <<- solve(inv) 
  } 
  
  getInverse <- function() {
    minv 
  }
  
  list(set = set, get = get,
       setInverse= setInverse,
       getInverse= getInverse)
}


## cacheSolve calcualates inverse of an matrix created by makeCacheMatrix function.
## It first checks if matrix inverse has been calculated. If its already been calculated
## it will return cached value otherwise it will create inverse of an matrix and return.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  print(class(x))
  ## Lookup inverse of matrix created in above function.   
  minv <- x$getInverse()
  ## Check if value returned is null or not if its null cache wasnt available calculate again.
  if(!is.null(minv)) {
    print("REturning cache values")
    return(minv)
  }
  ## This should retrun same matrix in data 
  data <- x$get()
  ## Calculate inverse of an matrix
  
  minv <- solve(data)
  ## Call setInverse to cache inverse for this object in cache
  x$setInverse(minv)
  ## Return inverse
  minv
}



