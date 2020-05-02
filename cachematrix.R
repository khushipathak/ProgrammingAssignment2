## Put comments here that give an overall description of what your
## functions do

# 1.  `makeCacheMatrix`: This function creates a special "matrix" object
# that can cache its inverse.

# 2.  `cacheSolve`: This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.


## Write a short comment describing this function

# Similar to the makeVector function given in the assignment description, this function
# will return a list of 4 functions to 
# - get the matrix
# - set the matrix
# - get the inverse
# - set the inverse
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse_matrix) inv <<- inverse_matrix
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function

# Similar to the cachemean function, this function will run an IF statement to find out if the matrix's
# inverse has already been calculated, and returns it if it exists.
# If it does not exist, the function will calculate the inverse and return that.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
