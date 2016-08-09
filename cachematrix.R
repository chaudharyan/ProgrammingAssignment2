##############################################################################################################################
# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse
# of a matrix rather than compute it repeatedly. 
# This file contains two functions makeCacheMatrix() and cacheSolve() which caches the Inverse of the matrix
# makeCachefunction () caculates and stores the Inverse of the matrix in the cache. 
# cachesolve() retreives the cached Inverse value if the matrix has not changed instead of computing it again. 
# Only if the cache is empty or the matrix has changed then cachesolve() computes the Inverse of the matrix.

##############################################################################################################################


# makeCacheMatrix() caculates and stores the Inverse of the matrix (passed as an argument x) in the cache.
# It builds a set of functions and returns the functions within a list to the parent environment. 
# It contains four functions set(), get() , setInverse() & getInverse() and also two data objects, x and m.

makeCacheMatrix <- function(x = matrix()) 

{
    
    m <- NULL
    set <- function (y)
    {
         x <<- y
         m <<- NULL

    }

    get <- function() x
    setInverse <- function (inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,setInverse = setInverse, getInverse = getInverse)
    
    
    
}


## cacheSolve()calculates the Inverse of the matrix created with the above function
## It first checks if the Inverse of the matrix  (when the matrix has not changed) has alrady been calculated. 
##If yes then it retrievs the inverse from cache. If not then it calculates the Inverse 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    

    m <- x$getInverse()

     if (!is.null(m))
     {
         message ("getting cached data")
         return (m)

     }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m


}

########################### Example to test the functions ###########################

# Define a small Matrix
matr1 <- matrix(1:4, nrow=2, ncol=2)

# myMatrix object prevents the memory consumed by makeCacheMatrix() from being released by the garbage collector.
myMatrix <- makeCacheMatrix(matr1)

myMatrix$get()

#There is no cached value here so Cachesolve() calculates the Inverse
cacheSolve(myMatrix)

# Cachesolve() gets the cached value during the second run as the matrix has not changed and cache is not empty
cacheSolve(myMatrix)


