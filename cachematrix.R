## These functions cache a matrix and returns inverse of matrix and caches it for further use.
##It checks if inverse is already calculated then it take the cached value of 
## inverse of matrix else it calculates inverse and caches the value
## 

## this function creates and caches the matrix and return a list containing 
## functions - set,get, setInv anf getInv

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
        
}


## this function checks if inverse of matrix is already calculate, 
##it returns the the cached inverse else it calculates the inverse of
##matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}
