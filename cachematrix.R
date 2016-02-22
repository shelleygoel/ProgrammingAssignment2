# Functions to create a matrix which is able to retrieve it's inverse 
# from the cache

# Create a cache matrix with methods to store data and inverse in cache
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set=set,get=get,
             setinverse=setinverse, 
             getinverse=getinverse)
}



# A function which retrieves inverse of a cached matrix from the cache
# if inverse is already calculated else calculates it
cacheSolve <- function(x, ...) {
        inv <- x$getinverse() # check if inverse present in cache
        
        # inverse is already calculated
        if (!is.null(inv)){
                message("getting cached data") 
                return(inv)
        }
        
        # inverse not present:calculate it
        data <- x$get()
        # diag is used to create an identity matrix, so ... arguments
        # can be passed in appropriate place
        inv <- solve(data, diag(dim(data)[1]), ...)
        
        # store inverse in cache
        x$setinverse(inv)
        
        inv
}
