## These functions start a two-step dance for inversing a data matrix using a cache tool
## Step to the left: creating a matrix
makeCacheMatrix <- function(x = matrix()) {
                        inv <- NULL
                        set <- function(y) {
                                x <<- y
                                inv <<- NULL 
                }
                get <- function() x
                setinverse <- function(inverse) inv <<- inverse
                getinverse <- function() inv
                list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
## Step to the right: introducing a cache tool
cacheSolve <- function(x, ...) {
              inv = x$getinverse()
              if (!is.null(inv)){
                  message("getting cached data")
                  return(inv)
              }
              data <- x$get()
              inv <- solve(data, ...)
              x$setinverse(inv)
              return(inv)
}
