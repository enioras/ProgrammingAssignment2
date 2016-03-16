
## The first function makeCacheMatrix create a inverse of a matrix in cache

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This second function is responsible for calculating the inverse of a matrix, 
## if the content has already been calculated and this cached, then it seeks the value that this 
## cached otherwise it calculates the new value and caches

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting data from cache")
                return(inv)
        }
        m <- x$get()
        inv <- solve(m, ...)
        x$setInverse(inv)
        inv
}

## Simulation

## Creating on matrix object
## > m <- makeCacheMatrix(matrix(c(4,6,7,8), 2, 2))

## First calc
## > cacheSolve(m)
##     [,1] [,2]
## [1,] -0.8  0.7
## [2,]  0.6 -0.4
##

## Second Calc with the data 
##> cacheSolve(my_matrix)
## getting data from cache
##     [,1] [,2]
##[1,] -0.8  0.7
##[2,]  0.6 -0.4


