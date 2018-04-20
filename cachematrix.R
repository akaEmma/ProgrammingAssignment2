## makeCache matrix creates and returns 4 functions and returns an
## object that can be used to store an inverse matrix. casheSolve
## caches and returns an inverse matrix or if possible gets it from the 
## cache instead of inverting it.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #initialize variables to be used later

        set <- function(y) { #push x and inv to the calling environment
                x <<- y
                inv <<- NULL # makes inv null to indicate that no inversion has happened
        }
        get <- function() x #anonymous function get the matrix that was passed here
        setinv <- function(inverse) inv <<- inverse #use and make function available outside
        getinv <- function()inv #anonymous function getinv
        list (set = set,
              get = get,
              setinv = setinv,
              getinv = getinv) # make them callable from outside
}

## check to see if the inverse has already been stored in the cache.
## if not, invert the matrix, cache it, and return it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' if it needs to be inverted here
        inv <- x$getinv() #use inv to store the answer to "was it done before?"
        if (!is.null(inv)) { #if inv is null, then the matrix hasn't been inverted before
                message("getting chached data") #get here if inv isn't null
                return(inv) #return the inverted matrix from the last time around
        }
        ## none of the below happens unless this is the first time we're inverting the matrix
        ## "solve" inverts an invertable (square, not collinear) matrix
        mymatrix <- x$get() #get the matrix originally passed
        inv <- solve(mymatrix, ...) #invert it and store the inversion in "inv"
        x$setinv(inv) #set the inverse in inv so we can check it later
}
