## These two function create a cacheable matrix type object 
##for which its inverse can be calculated efficiently

## WThis function creates the cacheable matrix-type object 
##and gives it useful attributes

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <- y
                inv <- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <- inverse 
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Returns a matrix that is the inverse of the cachaeble matrix object 'x'

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
