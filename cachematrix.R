## Usually matrix inversion is a costly computation. 
## Below 2 functions aims to cache matrix inversion to help reduce computation time for repeatedly performed calculations. 
## 
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##             If the inverse has already been calculated (and the matrix has not changed), 
##             then the cachesolve should retrieve the inverse from the cache



## Followwing function creates a special "matrix" that peforms below steps:
#     - set the value of a matrix
#     - get the value of the matrix
#     - set the value of the inverse
#     - get the values of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Following function calculates the inverse of the special "matrix" created with above function
## It perfoms below steps:
#     - checks to see if inverse has already been calculated
#         - if so, then it retrives the value from the cache and skips computation
#     - otherwise, it calculates matrix inverse 
#         - and sets the value of the inverse in cache via setsolve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}

## Code to test the functionality ------------------------------------------

# mat <- matrix(6:10, 2,2)
# solve(mat)
# 
# mc <- makeCacheMatrix(mat)
# cacheSolve(mc)
# cacheSolve(mc)
