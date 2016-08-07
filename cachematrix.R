
## makeCacheMatrix creates a special "matrix" object that can cache its revrse.  

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<- function(y) {
                x<<- y
                m<<- NULL
        }
        get <- function() x
        setmatrix <- function (solve) m<<- solve
        getmatrix <- function() m
        list (set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## This function computes the inverse of the "matrix" object created by makeCacheMatrix. 
## if the inverse has already been cached, it should retrive the inverse from the cache
## and print a message "getting cached data"

cacheSolve <- function(x, ...) {
        cacheSolve <- function(x=matrix(), ...) {
                m<-x$getmatrix()
                if(!is.null(m)){
                        message("getting cached data")
                        return(m)
                }
                matrix<- x$get()
                m<-solve(matrix, ...)
                x$setmatrix(m)
                m
        }
 
