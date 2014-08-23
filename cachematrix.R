## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##reapplying makeVector to store a matrix, set it's inverse and get it's inverse.
makeCacheMatrix <- function(x = matrix()) {  

    m <- NULL
    
    get <- function() { x } ##gets the matrix being passed in the function call
    setinverse <- function(solve) {m <<- solve} ##store the value of inverse on the first iteration
    getinverse <- function() {m} ##return the value of inverse on repeat iteration
    list(get = get, ##list of methods to be returned by makeCacheMatrix
         setinverse = setinverse,
         getinverse = getinverse)
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m <- x$getinverse() ##checks for cached value in makeCacheMatrix
    if(!is.null(m)) { ##if value returned is not NULL returns the cached value
      message("getting cached data")
      return(m)
    }
    data <- x$get() ##if the inverse is NULL, picks up the matrix from makeCacheMatrix
    if(det(data)==0) { ##error checking for singular matrix, returns the matrix passed in case singular is true
      message("cannot invert singular matrix")
      return(data)
    }
    
    m <- solve(data, ...) ##finds inverse of non singular matrix
    x$setinverse(m) ##passes the value back to makeCacheMatrix for storing
    m 
}
