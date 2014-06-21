## Here there are two functions. With the help of these two functions
## we are trying to calculate inverse of matrix. If we are trying to calculate
## the inverse of same matrix again then it give the output from the cache.
## Overall we are trying to save the time of the user.

## Below are two functions that are used to create a special object that stores
## a matrix and cache's its inverse. 
##The first function, makeCacheMatrix creates a special "matrix", and functions 
## 1.set the value of the Matrix
## 2.get the value of the Matrix
## 3.set the value of the Inverse
## 4.get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
 inverse<-NULL
  set<-function(y){
  x<<-y
  inverse<<-NULL
}
get<-function() x
setinverse<-function(solve) inverse<<- solve
getinverse<-function() inverse
list(set=set, get=get,
   setinverse=setinverse,
   getinverse=getinverse)

}

## The following function calculates the Inverse of the special "matrix"
## created with the above function. However, it first checks to see if
## the Inverse has already been calculated. If so, it gets the Inverse from
## the cache and skips the computation. Otherwise, it calculates the Inverse
## of the data and sets the value of the Inverse in the cache via the
## setinverse function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 
		 inverse<-x$getinverse()
    if(!is.null(inverse)){
      message("getting cached data")
      return(inverse)
    }
    matrix<-x$get()
    inverse<-solve(matrix, ...)
    x$setinverse(inverse)
	
    inverse
}
## Solution can be checked using following example
##demo<-makeCacheMatrix()
##demo$set(matrix(1:4,2,2))
##cacheSolve(demo)
##solution looks like
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
