## These functions accept as input a matrix and return the inverse
## 

## The makeCacheMatrix function creates a list object and stores the matrix and its inverse.  
## makeCacheMatrix function provides methods for the calling function cacheSolve()

makeCacheMatrix <- function(x = matrix()) {  # input x will be a matrix
        inv <- NULL     #  inv will be our 'inverse' and it's reset to NULL every 
        #    time makeCacheMatrix is called
        set <- function(y) {
                x <<- y  #original matrix value
                inv <<- NULL  #cached value initialized to NULL
        }
        #  note these next three functions are defined but not run when makeCacheMatrix is called.
        #   instead, they will be used by cacheSolve() to get values for x or for
        #   inv (inverse) and for setting the inverse.  These are usually called object 'methods'
        
        get <- function() { x }   # this function returns the value of the original matrix
        
        setinv <- function(solve)  { inv <<- solve }
        # this is called by cacheSolve() during the first cacheSolve()
        #  access and it will store the value using superassignment
        
        getinv <- function() { inv }    # this will return the cached value to cacheSolve() on
        #  subsequent accesses
        
        list(get = get,          #  This is accessed each time makeCacheMatrix is called,       
             setinv = setinv,  #   that is, each time we make a new object.  This is a list of 
             getinv = getinv, set = set)
        #   the internal functions ('methods') so a calling function
        #   knows how to access those methods.                            
}


## CacheSolve function accesses the object created by makeCacheMatrix function and returns
## a matrix that is the inverse

cacheSolve <- function(x, ...) {  # the input x is an object created by makeCacheMatrix()
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()               # accesses the object 'x' and gets the value of the inverse
        if(!is.null(inv)) {              # if inverse was already cached (not NULL) ...
                
                message("getting cached data")  # ... send this message to the console
                return(inv)                       # ... and return the inverse ... "return" ends 
                #   the function cacheSolve(), note
        }
        data <- x$get()        # we reach this code only if x$getinv() returned NULL
        inv <- solve(data, ...)   # if inv was NULL then we have to calculate the inverse using solve()
        x$setinv(inv)           # store the matrix inverse in x (see setinv() in makeCacheMatrix()
        inv               # return the inverse to the code that called this function

}
