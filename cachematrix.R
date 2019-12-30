## Put comments here that give an overall description of what your
## functions do

# These functions are heavily based in the example ones, refactoring the code
# as where able, so its structure is similar. Particularly, we reuse the
# code to create the matrix object, the get method, the set method, etc.
# What they do is create an object with an associated environment where we have
# 4 methods and two pieces of data. The methods are used to retrieve and set
# the inverse of the matrix fed to the object, and the data are the matrix and 
# its inverse.

# First, when created and assigned, the object is created with two important
# pieces of information: The data
# - inv: a NULL inverse
# (which is accessible via x$getinv()) and 
# - the original matrix
# via x$get()
# The methods:
# - set: to set up a matrix in the environment
# - get: to retrieve such matrix
# - setinv: to set up the an inverse in the 'inv' data
# - getinv: to retrieve such inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


# The second function, cacheSolve, interfaces with the methods in x to, first,
# retrieves retrieves the cached inverse from the environment of x via
# x$getinv(), and sees if the object has a calculated inverse (signaled by the
# fact that that if not, then x$inv() is NULL, then, if it's already calculated
# it retrieves the cached inverse present in x, if not, it calculates it via
# solve(x$get()) and then 'saves' it in the original object x with x$setinv().
#
# For a time I was worried about this requirement:
#
# "(...) If the inverse has already
# been calculated (and the matrix has not changed), then the cachesolve should
# retrieve the inverse from the cache."
#
# Because I was thinking about a way to create a flag to signal when the matrix
# has changed and introduce it in the logical requirements of cacheSolve.
# Then I realized that this is already taken care of with the fact that
# setting a new matrix with the $set() method already 'cleans' the internal
# calculated inverse (that is, reinitializes it to NULL), so 'cacheSolve' will
# find a NULL inverse when the matrix has changed. Therefore, the assignment was
# just an exercise on refactoring the given code.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    data <- x$get()
    inv <- x$getinv()
    if(!is.null(inv)){
        message("retrieving cached inverse")
        return(inv)
    }
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}