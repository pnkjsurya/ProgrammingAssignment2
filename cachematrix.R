## function makeCacheMatrix : This function creates a special "matrix" object that can cache its inverse.
## Argument x is a square invertible matrix
## function returns a list containing following 4 functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##  this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    
    ## 1. set the matrix
    set = function(y) {
        # use `<<-` to assign a value to an object in an environment 
        # different from the current environment. 
        x <<- y
        inv <<- NULL
    }
    
    ## 2. get the matrix
    get = function() x
    
    ## 3. set the inverse
    setinv = function(inverse) inv <<- inverse
    
    ## 4. get the inverse
    getinv = function() inv
    
    ## Returning list of above 4 functions
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Function cacheSolve, computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Argument x is outcome of execution from above function makeCacheMatrix()
## Function returns inverse of the original matrix input function to makeCacheMatrix()

cacheSolve <- function(x, ...) {
    inv = x$getinv()
    
    # if the inverse has already been calculated
    if (!is.null(inv)){
        # get it from the cache and skip the processing. 
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


## Function test, This function is not part of assignment , ut used for testing purpose only
## Argument mat is an invertible matrix
## Function calculates inverse of matrix mat1 twice using the above functions, and prints out the times it takes for both runs. 
## The first run should take longer than the second run because it actually calculates the inverse 
## while the second run only does a look-up from the cache, so it should be faster.
## Following commands can be used to run this function
## set.seed(1110201)
## r = rnorm(1000000)
## mat1 = matrix(r, nrow=1000, ncol=1000)
## test(mat1)

test = function(mat){
    
    temp = makeCacheMatrix(mat)
    
    start.time = Sys.time()
    cacheSolve(temp)
    diff = Sys.time() - start.time
    print(diff)
    
    start.time = Sys.time()
    cacheSolve(temp)
    diff = Sys.time() - start.time
    print(diff)
}
