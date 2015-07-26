##      makeCacheMatrix and cacheSolve

##      The first function, "makeCacheMatrix," creates a special "matrix" object
##      which can cache its inverse. It must:
##      1.      set the value of the matrix
##      2.      get the value of the matrix
##      3.      set the value of the matrix inverse
##      4.      get the value of the matrix inverse
##

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

##      If the matrix inverse is already calculated and cached,
##      it will notify you of that and pull that inverse matrix
##      from the cache.
##      ------------------------

##      The 2nd function, "cacheSolve," returns the inverse of a matrix created from
##      the makeCacheMatrix function.
##      If the cached inverse already has been calculated, cacheSolve retrieves it,
##      so there is no need to calculate it again. 
##      If it hasn't been calculated, "it"cacheSolve" computes the inverse matrix,
##      stores it in cache, and then returns it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("The inverse matrix was already in cache. Here it is:")
                return(inv)
        } else {
                inv <- solve(x$get())
                x$setinv(inv)
                return(inv)
        }
}
##      My check of the functions:
##      > x <- rbind(c(1, -2), c(-1/5, 1))
##      > c <- makeCacheMatrix(x)
##      > c$get()
##           [,1] [,2]
##      [1,]  1.0   -2
##      [2,] -0.2    1
##> cacheSolve(c)
##              [,1]     [,2]
##      [1,] 1.6666667 3.333333
##      [2,] 0.3333333 1.666667
##> cacheSolve(c) %*% c$get()
##The inverse matrix was already in cache. Here it is:
##            [,1] [,2]
##      [1,]    1    0
##      [2,]    0    1
