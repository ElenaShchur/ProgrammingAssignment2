##Inversing a matrix is a time-consuming operation especially for large matrices. In case we need to inverse the same matrix several times, it could make sense to cache its inverse. We introduce two functions which allow to cache matrix'x inverse instead of recomputing it.
   
## The first function, makeCacheMatrix creates a special object, which includes a matrix and the ability to store its inverse. Actually it's a list of 4 functions that	
## 1.	set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

 makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## The next function calculates the inverse of the "matrix" created with the above function. But at first it checks whether the inverse has already been calculated. If yes, it gets the inverse from the cache and skips the computation. If no, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

##Example:
#> x <- matrix(1:4, 2, 2)
#> mat <- makeCacheMatrix()
#> mat$set(x)
#> mat$get()
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> cachesolve(mat)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cachesolve(mat)
#getting cached data
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5 