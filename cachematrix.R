## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function


cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)                     
        i
}

x <- matrix( c(2, 4, 3, 1, 5, 6, 7, 3, 1), # the data elements 
                nrow=3,              # number of rows 
                ncol=3,              # number of columns 
                byrow = TRUE)

# Check for determinant and apply solve to get a hint of what we want
det(x)
solve(x)

# Now apply cacheSolve and see of the solution matches
x1 <- makeCacheMatrix(x)
cacheSolve(x1)
