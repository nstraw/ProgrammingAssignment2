# cachematrix.R
#
# makeCacheMatrix: 
# This function creates a special "matrix" object that can cache its inverse.
#
# cacheSolve: 
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), then 
# the cachesolve retrieves the inverse from the cache.


# get a matrix, compute its inverse and stores values into function local environment so 
# it can be retrieved "as-is" if already computed (like cached data)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y)
    {
        x <<- y
        inv <<- NULL # nullify inv if matrix x changes
    }
    get <- function()
    {
        x
    }
    setinv <- function(y)
    {
        inv <<- y
    }
    getinv <- function()
    {
        inv
    }
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


# use makeCacheMatrix object to get the inverse of x, either computed if it's a first-time
# call or directly retrieved from the existing cached value otherwise
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (is.null(inv))
    {
        print("1st call : compute inverse and store as cached data.")
        x$setinv(solve(x$get(), ...))
    } 
    else 
    {
        print("Inverse already computed, using cached data.")
    }
    x$getinv()
}
