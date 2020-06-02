## The next functions are developed to create a special object that stores a
## matrix and caches its inverse.

## This function creates a "matrix", that is really a list containing a function
## to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {
      x <<- y
      i <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) i <<- inverse
   getinverse <- function() i
   list(set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
           message("getting cached data")
           return(i) ## Return a matrix that is the inverse of 'x'
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i            ## Return a matrix that is the inverse of 'x'
}
