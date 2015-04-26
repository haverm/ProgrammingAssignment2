## To whom may concern:
## The code is commented on each line. It can do what is required by the Programming Assignment 2.
## You can test its correct behavior with the test described in: https://class.coursera.org/rprog-013/forum/thread?thread_id=127

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                       ##We will denote m as the inverse of the matrix
        set <- function(y) {            ##The same function in the code provided by the course. Set x to the value of y
                x <<- y                 ## & set the inverse to NULL
                m <<- NULL
        }
        get <- function() x             ##Get the original matrix X
        setinverse <- function(inverse) m <<- inverse ##Set the inverse of X
        getinverse <- function() m      ##Get the inverse of X
        list(set = set, get = get,      ##Group all functions in a vector
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getinverse() ##Get the inverse from the makeCacheMatrix function
        if(!is.null(m)) {   ##If is different from NULL
                message("getting cached data")
                return(m)   ##Return the cached data
        }
        data <- x$get()  ##Else, get the original matrix X
        m <- solve(data, ...) ##Solve function over data will do the inverse of x and save it in m
        x$setinverse(m)  ##This line saves the value of m in the makeCacheMatrix function.
        m       ## Return a matrix that is the inverse of 'x'
}
