## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## first we set our variable to NULL
    inv <- NULL
    ## the set function substitutes the matrix X with the matrix Y, and sets the value
    ## of inv to null
    set <- function(y){
        x <<- y
        inv <<- NULL
 }
  ## the function get retrieves the value of X
    get <- function() x
    
    ## setinv sets the variable inv to the value of inverse
    setinv <- function(inverse) inv <<- inverse
    
    ## getinv gets the value of inv
    getinv <- function() inv
    ## this las line creates a list of the above functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## the cacheSolve function returns the inverse of a matrix, 
## it first checks to see if there is a cached version, and if not
##calculates de result and returns it.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## it returns a value
    inv <- x$getinv()
    if(!is.null(inv)){
        message("Retrieving cached data")
        return(inv)
    }
    ## if inv is null, it calculates the inverse of the matrix.
    data <- x$get()
    inv <- solve(data)  
    x$setinv(inv)
    inv
    ## this returns a matrix that is the inverse of 'x'
   
}
