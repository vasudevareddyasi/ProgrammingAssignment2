## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL               ## setting null for inverse of the matrix 
  set<- function(y) {
    x<<-y
    inv<<-NULL
  }
  get <- function () {x}
  setinverse <- function(inverse) {inverse<<-inverse} ##setting the value of inverse matrix
  getinverse <- function() {inv} ##get the value of inverse matrix
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null (inv)) {
    message("getting cached data")
    return(inv)   ##returning the inverse value after "getting cached data"
  }
  mat<-x$get()
  inv<-solve(mat,...)   ##getting the inverse of the matix
  x$setinverse(inv)     ## set the inverse value to inv     
  inv
}
