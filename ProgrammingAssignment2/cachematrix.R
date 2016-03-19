## The first function, makeCacheMatrix creates a matrix, which is really a list containing a function to
## set and get the value of this matrix, and set and get the value of the inverse of this matrix

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    setInverse<-function(inverse) inv<<- inverse
    getInverse<-function() inv
    list(set=set, get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


## The function cacheSolve calculates the inverse of the matrix created with makeCacheMatrix. 
## However, it first checks to see if the inverse (inv) has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets this value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x=matrix(), ...) {
    inv<-x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    matrix<-x$get()
    inv<-solve(matrix, ...)
    x$setInverse(inv)
    inv
}
## Return a matrix that is the inverse of 'x'
