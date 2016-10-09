## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #cached inverse of matrix
        inv <- NULL
        
        # getter and setter for matrix
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv<<- inverse 
        getInverse <- function() inv
        
        #return list of functions for matrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        #return cached matrix inverse if it's been already computed
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        
        #cache inverse
        x$setInverse(inv)
        
        #return inverse of matrix
        return(inv)
}

# To test my result
## > my_m <- makeCacheMatrix(matrix(1:4, 2, 2))
## > my_m$get()
## [,1] [,2]
## [1,]    1    3
#  [2,]    2    4
## > my_m$getInverse()
#  NULL
## > cacheSolve(my_m)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##  > cacheSolve(my_m)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5