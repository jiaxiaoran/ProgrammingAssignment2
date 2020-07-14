## The first function makeCacheMatrix() set up an "environment" of functions which 
## return a list of four functions including set(), get(), setinverse(), and getinverse()
## The purpose of this function is for the second function (described next) to call upon
## It also initialize two objects - the x matrix and the placeholder i for inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The second function determines whether the inverse matrix has already been cached or not
## If it is already cached, then call getinverse() and retrieve the cached inverse, which will be returned
## If it is not cached, then get() will be called and the inverse will be calculated, then set (or cached)
## by calling setmean()

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}

## Finally, to test the functions, I run the following codes:
x = matrix(1:4, nrow = 2, ncol = 2)
p <- makeCacheMatrix(x)
cacheSolve(p)    ## The results, if run for the first time, will be 'calculated'
cacheSolve(p)    ## The results, if run for the second or more time, will show 'getting cached data'

