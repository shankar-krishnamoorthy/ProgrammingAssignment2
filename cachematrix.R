#makeCacheMatrix creates special object that caches
#matrix inverse to avoid repeated computation.
makeCacheMatrix <- function(m=matrix()) {
        i <- NULL
        
        #set the matrix
        set <- function(y) {
                m <<- y
                i <<- NULL
        }
        #returns the matrix
        get <- function() m
        
        #caches the inverse.
        setinverse <- function(inverse) {
                i <<- inverse
        }
        # retrieves the cached inverse. 
        # NULL will be returned, if cache is not found
        getinverse <- function() i
        
        #packs all functions into a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#function to get the inverse. If inverse is already computed
#will return the cached copy. Otherwise does fresh calculation.
cacheSolve <- function(x,...) {
        i <- x$getinverse()
	#if cached, simply return it
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        res <- x$get()
        #compute inverse
        i <- solve(res, ...)
        x$setinverse(i)
        i
}

#usage:
#m<-makeCacheMatrix(matrix(1:4,2,2))
#cacheSolve(m)
