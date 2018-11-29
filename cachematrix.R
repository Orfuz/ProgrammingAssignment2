
## Basically, this makeCacheMatrix has to function like makeVector() example we came across in the presentation of the assignment

makeCacheMatrix <- function(x = matrix()) {
    inv<- NULL
    ## We define the set function with "y" argument in order not to be confused
    
    set <- function(y) {
        ## We assign the "y" argument to "x" in the parent environment and  "NULL" to "inv" in the parent environment
        x<<- y
        inv<<- NULL
    }
    get <- function() x
    setinverse<- function(solve) inv<<- solve
    getinverse<- function() inv
    ##We organise as a list in order to access function with a $ later on in the cacheSolve function
    ##Elements in the list is reated such that what's in the left part is the name and what's in the right part is the value
    list(set = set, ## So here, we assign the name "set" to the "set()" function
    get = get, ## Same here with get and get()
    setinverse = setinverse, ##Idem
    getinverse = getinverse) ##Idem
    
}


cacheSolve <- function(x, ...) {
    inv <- x$getinverse() ##We try to get an inverse of the matrix passed as the argument of the function
    if(!is.null(inv)) ##If the value is not equal to NULL, we return the cached inverse of the matrix
    {
        message("Getting cached data")
        return(inv)
    }
    ## If the value is equal to NULL, then we proceed to calculate the inverse of the matrix.
    data <- x$get() ## First we get the matrix
    inv <- solve(data, ...) ##Then we solve the inverse of the matrix
    x$setinverse(inv) ## Then we set the inverse of the matrix
    inv ##Finally, we return the value of the inverted matrix
}
