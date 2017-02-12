## My functions "makeCacheMatrix and "cacheSolve" calculate the inverse of a matrix and cache it. If inverse of the same  matrix was already calculated say in a for loop, the cached inverse is returned ie not recalculated
## 

## The makeCacheMatrix function takes a matrix "x", calculates its inverse and caches the inverse of matrix (x)

makeCacheMatrix <- function(x = matrix()) {
	v<-NULL
        set<-function(z){
                x<<-z
                v<<-NULL
        }
        get<-function() x
        setinv<-function(solve) v<<-solve
        getinv<-function() v
        list(set = set,get = get,
             setinv = setinv,
             getinv = getinv)

}


## The "cacheSolve" function takes the "makeCacheMatrix" and uses it to determine if the inverse of the matrix already exits as a cached object and returns it, otherwise calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        v<-x$getinv()
        if(!is.null(v)){
                message("getting cached data")
                return(v)
        }
        data<-x$get()
        v<-solve(data,...)
        x$setinv(v)
        v

}


