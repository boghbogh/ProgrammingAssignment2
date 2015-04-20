## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function stored a variable in Diff env compare to execution env. 
## Also create list of functions required to retrieve it.
makeCacheMatrix <- function(x = matrix()) {
        memoryObject<-NULL
        set<-function(cahedInverse)
        {
                x<<-cashedInverse
                memoryObject<<-NULL
        }
        get<-function() x
        setInverse<-function(Inverse) memoryObject<<-Inverse
        getInverse <- function() memoryObject
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## Write a short comment describing this function
## This function first check if this matrix inverse is in the cache.
##if this inverse is calculated before then return it directly. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        memoryObject <- x$getInverse()
        print(memoryObject)
        if(!is.null(memoryObject)) {
                message("getting cached data")
                return(memoryObject)
        }
        data <- x$get()
        memoryObject <- solve(data, ...)
        x$setInverse(memoryObject)
        memoryObject
}
