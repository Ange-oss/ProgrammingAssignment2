
#makeCacheMatrix creates a special "matrix" object that can cache its inverse.
#Set the value of the Matix
makeCacheMatrix <- function(x = matrix()) {
        inversematrix <- NULL
        set <- function(y) {
                x <<- y
                inversematrix <<- NULL
        }
        #get the value of the Matrix
        get <- function()x
        #set the value of the inverse
        setinverse <- function(inverse) inversematrix <<- inverse
        #get the value of the inverse
        getinverse <- function() inversematrix
        list(set = set, get = get, setinverse =setinverse, getinverse = getinverse)

}




## cacheSolve: This function computes the inverse of the 
##special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix 
##has not changed), then the cachesolve should retrieve the 
##inverse from the cache.

cacheSolve <- function(x, ...) {
        inversematrix <- x$getinverse()
        if(!is.null(inversematrix)){
                message("getting cached data")
                return(inversematrix)
        }
        data <- x$get()
        inversematrix <- solve(data,...)
        x$setinverse(inversematrix)
        inversematrix
        
}
