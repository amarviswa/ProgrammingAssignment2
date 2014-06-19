################################################################################
## 
## Name of the below function is makeCacheMatrix. The input parameter for this function is a matrix and  
## the output of this function is inverse of the matrix. 
## The first element is a function to cache the matrix if it is not cached alreday. 
## The second element is a function that echoes the cache matrix, and the third element provides the function
## this function returns inverse matrix either from the cache or from direct computation
## depending on whether the matrix has already been cached or not. 
##
################################################################################

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        ## Assigning Value to Parent Enviromnent 
        x<<-y
        m<<-NULL
    }
    get<-function() x
    ## Set Matrix Function
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}


## Name of the below function is cacheSolve. This function check if chched data is available or not. 
## If cashed data is available it will return from cache other wise it will call solve function and
## retrun the inverse matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
###################################TESTINGS TIPS ################################
## Create the Matrix.
##
##a<-makeCacheMatrix()

##Initialize the Matrix
##a$set(matrix(1:4,2,2)

## Retrieve/display the matrix part of the object
## a$get()

## Test the inverse by calling below function
##cacheSolve(a)

##Calling againg below function
##cacheSolve(a)
##It should retrun the cached data 

##
################################################################################
