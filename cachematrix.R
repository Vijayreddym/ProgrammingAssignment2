##
## caching the inverse of a matrix rather than compute it repeatedly 
##

##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) mi <<- solve
        getInverse <- function() mi
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mi <- x$getInverse()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data, ...)
        x$setInverse(mi)
        mi
        
}

TestInverse <- function(Testmat)
{
        ##y<- matrix(c(1,-1,1,1), nrow = 2)
        
        tempMat = makeCacheMatrix(Testmat)
        start.time = Sys.time()
        cacheSolve(tempMat)
        dur = Sys.time() - start.time
        print(dur)
        
        start.time = Sys.time()
        cacheSolve(tempMat)
        dur = Sys.time() - start.time
        print(dur)
        

}

SetMatrix <- function()
{
        set.seed(100000)
        r = rnorm(10000000)
        Testmat = matrix(r, nrow=1000, ncol=1000)
        TestInverse(Testmat)
        
}

