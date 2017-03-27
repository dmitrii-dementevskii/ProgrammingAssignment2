## The code below are used to reduce computational needs of the inverting of a matrix
## by caching of results

## Function to save or get the inverse results

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <-function(y){
        x <<-y
        inverse <<-NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## this function returns cached iverse or calculate iverse and saves is in cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
            message("getting cached inverse")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
