## Write a short comment describing this function
##The following function takes a matrix x, creates a special object matrix, computes its inverse using solve as inv which is
##initialized to zero and stores it in cache using set.
makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){             ##set the value of matrix
                x<<-y
                inv<<-NULL
        }
        get<-function()x               ##get the value of matrix
        setinverse<-function(solve) inv<<-solve ##set the value of inverse
        getinverse<-function()inv     ##get the value of inverse
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}        


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
       inv<-x$getinverse()
       if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data, ...)
        x$setinverse(inv)
        inv
}

