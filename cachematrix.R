## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv_result <- NULL
    set <- function(y){
        x <<- y
        inv_result <<- NULL
    }
    get <- function() x
    set_inv <- function(inv) inv_result <<- inv
    get_inv <- function() inv_result
    
    list(set = set, get = get,
         set_inv = set_inv, 
         get_inv = get_inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv_result <- x$get_inv()
    if (!is.null(inv_result) ) {
        message("getting cached data")
        return(inv_result)
    }
    data <- x$get()
    inv_result <- solve(data)
    x$set_inv(inv_result)
    inv_result
}
