
## Below are two functions that are used to create a special object that stores a matrix and caches its inverse.


## The first function, makeCacheMatrix  creates a special "vector", which is really a list containing functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # create 4 functions: set, get, setinverse, getinverse
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    # Create list of functions we just created
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## The following function calculates the inverse of the matrix created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # Grab original matrix
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## Sample Test Cases

# temp <- matrix(5:8,nrow=2,ncol=2) # Create a temp matrix
# 
# funcvector<-makeCacheMatrix(temp) # Generate vector of functions 
# 
# funcvector$get()                  # Get original matrix
# 
# cacheSolve(funcvector)            # Computes, caches, and returns    matrix inverse
# 
# funcvector$getinverse()           # Get inverse
# 
# cacheSolve(funcvector)            # Test for caching
# 
# temp2 <- matrix(1:4,nrow=2,ncol=2) # Create a second temp matrix
# 
# funcvector$set(temp2) 
# 
# cacheSolve(funcvector)            # Test for caching