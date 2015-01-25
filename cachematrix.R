#Matrix inversion is usually a costly computation and there may be some benefit to caching 
#the inverse of a matrix rather than computing it repeatedly (there are also alternatives to 
#matrix inversion that we will not discuss here). 
#The assignment is to write a pair of functions that cache the inverse of a matrix.

#The first function, makeCacheMatrix() creates a special "matrix", which is really a 
#list containing a function to

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the matrix inverse
# 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getinverse<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getinverse=getinverse)
}
#The following function calculates the mean of the special "matrix" created with the 
#above function. However, it first checks to see if the matrix inverse has already been 
#calculated. If so, it gets the matrix inverse from the cache and skips the computation. 
#Otherwise, it calculates the matrix inverse of the data and sets the value of the inverse 
#in the cache via the setmatrix function.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}