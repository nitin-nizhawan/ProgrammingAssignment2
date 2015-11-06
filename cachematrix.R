## Functions below have been implemented as paritial fulfilment of the 
## R Programming course in Data Science Specialization for Assignment 2

## Function makeCacheMatrix below allow creation of a wrapper matrix object
## which can store value in its cache.
## Function cacheSolve when passed and object return from makeCacheMatrix
## calculates inverse of matrix if no cache is found else returns cached value

## makeCacheMatrix function
## creates a wrapper object of passed
## matrix object. Cache create is store in 
## function scope and is updated using "<<-" operator 
## returned object is list with following fields
## setinv - sets the inverse in cache
## getinv - gets the inverse from cache
## set - sets the matrix
## get - gets the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get=get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve function
## return inverse of the CacheMatrix passed as argument
## it calls "solve" function to calculate inverse
## if inverse if not found in cache otherwise
## it returns cached value
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
            message("getting cached data")
            return (inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv        
}
