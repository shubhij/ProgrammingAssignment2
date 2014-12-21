##The following function takes a matrix x, creates a special object matrix, computes its inverse using solve as inv which is
##initialized to zero and stores it in cache using set.
makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){                                                  ##set the value of matrix
                x<<-y
                inv<<-NULL
        }
        get<-function()x                                                   ##get the value of matrix
        setinverse<-function(solve) inv<<-solve                            ##set the value of inverse
        getinverse<-function()inv                                          ##get the value of inverse
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}        


##The following function calculates the inverse of the matrix but first it checks if there is an inverse value in the cache 
##and does not compute inverse if its in cache

cacheSolve <- function(x, ...) {
       inv<-x$getinverse()                        ##get the inverse value from cache if its present and returns null otherwise
       if(!is.null(inv)){
                message("getting cached data")    ##if inv does not return null, it gets cached value of inverse
                return(inv)
        }
        data<-x$get()                            ##if inv returns null, data is taken using get
        inv<-solve(data, ...)                    ##inverse of data is computed which is not in cache
        x$setinverse(inv)                        ## inverse computed is stored in cache
        inv                                      ##value returned
}

