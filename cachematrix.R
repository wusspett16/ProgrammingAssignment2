## the below 2 functions are separated by PART 1 and PART 2.  The purpose of
## "makeCacheMatrix" is to:
##      1) test whether the incoming data is NULL
##      2) if the incoming data is new, it is stored (cached)
## The purpose of "cacheSolve" is to:
##      1) test if data has changed
##      2) if no change, returns cached value
##      3) if there is new data, it calculates the new inverse
##
## PART 1: makeCacheMatrix
## The function "makeCacheMatrix" creates a special "matrix" object that 
## can cache its inverse.
## It does the following in order:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(incoming)
  {
    x <<- incoming
    message("value has changed. clearing cache...")
    m <<- NULL
  }
  get <- function() { x }
  setMatrix <- function(solve) { 
    message("caching calculated inverse matrix")  
    m <<- solve
  }
  getMatrix <- function() m
  list(set = set, get = get, 
       setMatrix = setMatrix, getMatrix = getMatrix)
}

## PART 2: cacheSolve
## The function "cacheSolve" computes the inverse of the
## special "matrix" returned by makeCacheMatrix above. If
## the inverse was already calculated, it pulls the return 
## value from the cache.

cacheSolve <- function(x, ...) {
  m<-x$getMatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  message("cache not found. calculating inverse of matrix...")
  data<-x$get()
  m<-solve(data, ...)
  x$setMatrix(m)
  return(m)  
}

## Examples:
## > x <- matrix(50:53, 2, 2)
## > m <- makeCacheMatrix(x)
## > cacheSolve(m)
## cache not found. calculating inverse of matrix...
## caching calculated inverse matrix
##       [,1] [,2]
## [1,] -26.5   26
## [2,]  25.5  -25
## > cacheSolve(m)
## getting cached data
##       [,1] [,2]
## [1,] -26.5   26
## [2,]  25.5  -25
## > m$set(matrix(25:28, 2, 2))
## value has changed. clearing cache...
## > cacheSolve(m)
## cache not found. calculating inverse of matrix...
## caching calculated inverse matrix
##      [,1]  [,2]
## [1,]  -14  13.5
## [2,]   13 -12.5
## > cacheSolve(m)
## getting cached data
##      [,1]  [,2]
## [1,]  -14  13.5
## [2,]   13 -12.5