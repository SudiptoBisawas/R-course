## Assumed the matrix always inversible.1.set the value of the matrix.2.get the value of the matrix
## 3.set the value of the inverse.4.Get the value of the inverse.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
set<-function(z){
x<<-z
inv<<-NULL
}
get<-function(){x}
setinverse<-function(inverse){inv<<-inverse}
getinverse<-function(){inv}
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      inv<-x$getinverse()
if(!is.null(inv)){
message("getting cached data")
return(inv)
}
m<-x$get()
inv<-solve(m,...)
x$setinverse(inv)
inv
}
