## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ##Inversa will be the variable which will contain the inverse of the matrix.
    ##Inicial value=NULL
    inversa<-NULL
    ##Defining a function to set the value of the matrix we want to invert
    set<-function(y){
        x<<-y
        inversa<<-NULL
    }
    ##Getting the matrix "x"
    get<-function() x
    ##Setting the inverse of the matrix to "inversa"
    setinverse<-function(solve) inversa<<-solve
    ##Getting the inverse of the matrix "x", called "inversa"
    getinverse<-function() inversa
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    ##Setting "inversa" to the what is the variable "getinverse" of the list
    ##from the previous function
    inversa<-x$getinverse()
    ##If there is already a value (i.e. a matrix) it means that it has been 
    ##previously calculated, then we get the value from the cache
    if(!is.null(inversa)){
        message("getting cached data")
        return(inversa)
    }
    ##If the inverse of the matrix has not been calculated yet, we calculate it.
    ##Getting the matrix
    matrix<-x$get()
    ##Calculating its inverse
    inversa<-solve(matrix,...)
    x$setinverse(inversa)
    inversa
}
