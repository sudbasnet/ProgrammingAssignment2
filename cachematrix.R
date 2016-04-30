## This function creates a special "matrix" object that can cache its inverse.
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL ## initialize iv as NULL to later hold the inverse of matrix
  set <- function(y){
    x<<-y 
      ##since x was declared outside of function set, double arrow is used '<<-' to assign value to the parent environment
    
    iv<<- NULL
      ## if the matrix is new we'll make iv of parent NULL
  }
  get <-function() x 
    ## this will just return the matrix x
  
  setivCache <- function(solve) iv <<- solve 
    ##taking inverse and assigning it to iv of the parent environment
  
  getivCache <- function() iv 
    ## simply return the value of iv where it is called
  
  list (set=set, get=get, setivCache=setivCache, getivCache=getivCache)
    #output of this function is a list that we can later use with the '$' operator
 }


## The function cacheSolve calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setivCache function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  iv<-x$getivCache()
  
  if (!is.null(iv)) {
      ##pull cached data if iv already exists, ie; exists in cache
    
    message("getting cached data")
    return(iv)
  }
  
  data<- x$get()
  iv<-solve(data,...)
  
  x$setivCache(iv)
  iv
  
}
