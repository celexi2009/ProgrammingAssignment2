# RSJ
# Programming assignment 3
# Two functions: makeCacheMatrix and cacheSolve
# plus a set of tests commented out at the end. 

# makeCacheMatrix; pretty simple, just change a bit from the mean 
# cache example.
# This function, `makeCacheMatrix` creates a special "vector", which is
# really a list containing a functions 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL
    set <- function(y) 
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}
  

# cacheSolve;
# This function, `cacheSolve`checks to see if an inverse is already 
# cached, if not, it calculates it and caches it, and returns the result
# either way.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    #we don't have an inverse so get it. 
    if (is.null(m))
    {
      dataToCache <- x$get()
      m <- solve(dataToCache, ...)
      x$setinverse(m)
    } 
    # no need for an else here; we had the inverse cached.
    m
}

# testing - uncomment to run: 
# use a random number to avoid the 
# Lapack dgesv: system is exactly singular error 
# set.seed(51503)
# testMatrix <- makeCacheMatrix(matrix(sample(1:9), 3, 3))
# testMatrix$get()
# testMatrix$getinverse()
# cacheSolve(testMatrix)
# testMatrix$getinverse()



