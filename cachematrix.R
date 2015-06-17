## Put comments here that give an overall description of what your
## functions do:
## Create a object contains the matrix and its inverse which appears 
## first time. This is a time-saving design for a time-consuming matrix.

## Write a short comment describing this function:
## "makeCacheMatrix" creates an object grabs information about a 
## matrix and its inverse ."set" creates  the object; 
## "get" retrieves the matrix; "setinverse"  stores the value of
## its inverse; "getinverse" retrieves the inverse if it has been calculated
## (if not it is set to NULL);

makeCacheMatrix<- function(x=matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setInverse<- function(i) m<<-i
        getInverse<-function()m
        list(set=set, get=get, 
             setInverse=setInverse, getInverse=getInverse)
        
}


## Write a short comment describing this function:
## "cacheSolve" calculates the inverse of  objects created by 
## "makeCacheMatrix". If the inverse of the object has never 
## been calculated yet, it applies solve to the matrix and store
## the inverse in the object through "setinverse". If the  inverse
## has been obtained before, it retrieves it from the stored value
## in the object through "getinverse".

cacheSolve<-function(x, ...){
        m<-x$getInverse()
        if(!is.null(m)){
                message("getting cached data!")
                return(m)
        }
        data<-x$get()
        m<-solve(data)
        x$setInverse(m)
        m
}