## These functions will take a matrix as a value, and cache or store the inverse of that
## matrix, making this especially usefull for loops or large matricies.  


## the makeCacheMatrix stores returns a list of functions that can be used to 
## set and call a matrix or calculate its inverse.  The inverse is cached inorder to 
## speed up re-calculations if required.



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
   set<-function(y){
   x <<- y
  m <<- NULL
}
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## cacheSolve will return the inverse of a matrix after first checking that the
## inverse has not already been cached by the previous function.  

cacheSolve <- function(y, ...) {
  m<-y$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  mat<-y$get()
  m<-solve(mat, ...)
  y$setinverse(m)
  m
}
        ## Return a matrix that is the inverse of 'x'

x<- matrix(2:5, nrow=2, ncol=2)

b<- makeCacheMatrix(x)

cacheSolve(b)


