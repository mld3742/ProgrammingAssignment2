######################################################################
## Matrix inverse cache functions 
##
## Looking for values in cache can save time over using CPU

######################################################################
# makeCacheMatrix(x = matrix())
# Description:
# Create a list of functions and an envrionment to store a matrix and its inverse.

# Arguments:
# x: a squre invertible numeric or complex matrix 

# Returns: 
# list of functions (set, get, set.inverse, get.inverse)

# To use function, run the following (as an example):
# source("cachematrix.R")
# x <- replicate(1500,rnorm(1500))
# z <- makeCacheMatrix(x)
makeCacheMatrix <- function(x = matrix()) {
        # For the <<- assignment operator to work properly x and inv.cache have to exist .
        # Otherwise, inv.cache would get created in the global environment when
        # <<- is called in the set function. NULL is a good sentinel to
        # ensure we don't mistake existance for a cached value.
        inv.cache <- NULL
        
        set <- function(y) {
                # `<<-` sets the values of x and inv.cache in makeCacheMatrix's
                # environment and not this function
                x <<- y
                inv.cache <<- NULL
        }
        get <- function() x
        set.inverse <- function(i) inv.cache <<- i
        get.inverse <- function() inv.cache
        
        return (list(set = set, 
                     get = get,
                     set.inverse = set.inverse,
                     get.inverse = get.inverse
        )
        )
}
## end of makeCacheMatrix
######################################################################

########################################################################
# cacheSolve(z,verbose = FALSE, ...)
# Description:
# When passed a makeCacheMatrix value computes its inverse or fetches it from 
# memory if available.

# Arguments:
# z: the return value from makeCacheMatrix
# verbose: logical.  If TRUE, messages are printed when calculating or accessing
#          the cached values.

# Returns:
# The inverse of the matrix stored in z

# Usage:
# > system.time(cacheSolve(z))
# user  system elapsed 
# 6.212   0.015   6.225 
# > system.time(cacheSolve(z))
# getting cached data
# user  system elapsed 
# 0       0       0 
cacheSolve <- function(z, verbose=FALSE,...) {
        # attempt to fetch the inverse from cache
        inverse <- z$get.inverse()   
        
        # ... if the inverse exists
        if(!is.null(inverse)) {
                
                if(verbose) message("getting cached data")
                return(inverse)
                
        } else {
                
                # otherwise, calculate it once and store it
                if(verbose) message("caclulating and caching inverse for first time")
                
                data <- z$get()
                inverse <- solve(data, ...)
                z$set.inverse(inverse)
                
                return(inverse)
                
        }
}