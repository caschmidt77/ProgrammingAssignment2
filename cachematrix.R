# These functions work as a pair, finding and then caching the inverse of a matrix.
# By caching the data, if the same object is subsequently passed, the solution is retrieved
# from the cache rather than using costly processing time.

# The argument passed to this first function, makeCacheMatrix is a matrix for which we seek
# to find the inverse.  This function does not solve the inverse, but rather returns a list
# which is then passed as the argument of the second function cacheSolve, discussed below. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y) {
                x <<- y
                inv <<- NULL
        }
        get <- function () x
        setinverse <- function (inverse) inv <<- inverse
        getinverse <- function () inv
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# The object returned from the first function, makeCache Matrix is passed as the argument of this
# second function, cacheSolve.  cacheSolve returns the inverse of the matrix originally passed
# to makeCacheMatrix by a two step process.  First, if the same argument has been previously
# passed to cacheSolve, it will return the inverse from the cache, saving costly computation time.
# If the inverse is not already held in the cache, the function solves for the inverse and then 
# caches the result for future use.  If the object is then passed again, the inverse is retrieved from 
# the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse ()
        if (!is.null(inv)) {
                message ("retrieving cached data")
                return (inv)
        } else {
        data <- x$get ()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        }
}

#running a trial using above functions to validate operation
trial_matrix1 <- matrix (c(4,6,2,5), 2, 2) #create matrix
trial_matrix1 #print matrix purely for illustration
trial1 <- makeCacheMatrix(trial_matrix1) #creating list described in makeCacheMatrix description above
trial1 #print list purely for illustration
cacheSolve(trial1) #passing list into cacheSolve for first time - note absence of 
                    # message "retrieving cached data"
cacheSolve(trial1)  #passing list into cacheSolve for second time - note presence of
                    # message "retrieving cached data"

#checking solution
trial_matrix1 %*% cacheSolve(trial1)
cacheSolve(trial1) %*% trial_matrix1

#running second trial
trial_matrix2 <- matrix (c(4,7,11,2,4,1,9,5,7), 3, 3)
trial_matrix2
trial2 <- makeCacheMatrix(trial_matrix2)
trial2
cacheSolve(trial2)
cacheSolve(trial2)

#checking inverse correctly calculated on second trial
trial_matrix2 %*% cacheSolve(trial2)
cacheSolve(trial2) %*% trial_matrix2
