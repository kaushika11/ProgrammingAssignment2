##This is an assignment to cache potentially time-consuming computations.The  <<- operator is used 
##which can be used to assign a value to an object in an environment that is different from the current environment
##Matrix inversion is usually a costly computation and there may be some benefit to 
##caching the inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse
##a.set the value of this matrix.
##b.get the value of the matrix.
##c.set value of the inverse of the matrix.
##d.get the value of the inverse of the matrix

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


##The below function returns the inverse of the matrix.
##It first checks if the inverse has already been calculated.
##If so it gets the inverse from the cache and skips the computation.
##Otherwise, it calculates the inverse of the matrix and sets the value of the mean in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

##Program execution
##> x = rbind(c(1,-0.5),c(-0.5,1))
##> m = makeCacheMatrix(x)
##> m$get()
##> m$get()
##     [,1] [,2]
##[1,]  1.0 -0.5
##[2,] -0.5  1.0
##> cacheSolve(m)
##          [,1]      [,2]
##[1,] 1.3333333 0.6666667
##[2,] 0.6666667 1.3333333
##> cacheSolve(m)
##getting cached data.
##          [,1]      [,2]
##[1,] 1.3333333 0.6666667
##[2,] 0.6666667 1.3333333
##> 
