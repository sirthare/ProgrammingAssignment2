## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function to set, get,setinv and getinv

makeCacheMatrix <- function(x = matrix()) {
    inv<- NULL  ##inverse as NULL
    set<- function(y){
      x<<- y
      inv<<- NULL
    }
    get<- function() {x}  ##to get matrix x
    setInverse<- function(inverse) {inv<<- inverse} ##set inverse matrix
    getInverse<- function() {inv}  ##get inverse matrix
    list(set = set, get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve is a function to use the cache data.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse() ##inverse matrix of x
        if(!is.null(inv)){  ##if inverse has been calculated get it from the cache
          message("getting cached data")
          return(inv)
        }
        mat<- x$get()
        inv<- solve(mat, ...) ##to compute the inverse of a matrix
        x$setInverse(inv)
        inv
}
