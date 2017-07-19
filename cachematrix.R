## Put comments here that give an overall description of what your
## functions do

## makeCacheMatric is a function that creates a matrix where a 
#previously inverted matrix can be stored.

##The set function allows the stored matrix to be changed without
#rerunning the makeCachedMatrix function. and changes the variable
#inv to NULL to indicate that the correct inverse matrix is no
#longer stored within the cached matrix.

##The get function retrives and stores the output of he CacheSolve
#function or, in the case where the matrix is set manually, the x
#variable from the set function.

##The setinv function allows the user to set the inverse matrix manually
#rather than use the cacheSolve function.

##The get inv function retrives and stores the output of he CacheSolve
#function or, in the case where the inverse is set manually, the inv
#variable from the setinv function.

##The cacheSolve function checks to see if an inverse matrix is stored
#for matrix (x).  If it is it returns taht cached matrix.
# If it isn't, the function solves for the inverse of a matrix (x)and 
#stores the new matrix and inverse matrix in the previously created 
#cache matrix variable

makeCacheMatrix <- function(x = matrix()) {
  inv <-NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  } 
  
  get <- function() x
  
  setinv <- function (invmatrix){
    inv <<- invmatrix
  }
  
  getinv <- function() inv
  
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #test <- makeCacheMatrix(x)
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <-solve(data)
  x$setinv(inv)
  inv
}
