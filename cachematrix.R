## A matrix inverse of an invertible matrix is calculated and stored in memory for a subsequent 
## faster access to its value. 
##
## Usage, e.g.:
## 
## mat_obj<-makeCacheMatrix(matrix(c(4,2,7,6),2,2))
## cacheSolve(mat_object)
## cacheSolve(mat_object)   
## 


## makeCacheMatrix function takes a matrix as its input parameter(an invertible matrix is assumed), 
## creates a matrix object and simply defines object methods for matrix object manipulation.

makeCacheMatrix <- function(x = matrix()) {
    ## matrix inverse is set to NULL everytime a new matrix object is created 
    inv_m <- NULL
    ## set_matrix object method is defined for resetting the existing matrix object with a new value, 
    ## its respective matrix inverse is also reset when the set_matrix function is called   
    set_matrix <- function(y) {
        x <<- y
        inv_m <<- NULL
    }
    ## get_matrix object method is defined for returning source/original matrix value
    get_matrix <- function() {
        x
    }  
    ## setinv_matrix object method is defined for storing a matrix inverse in cache
    setinv_matrix <- function(solve) {
        inv_m <<- solve
    }    
    ## getinv_matrix object method is defined for returnng a matrix inverse object from cache
    getinv_matrix <- function() {
        inv_m
    }
    ## a list of 4 matrix object methods is published 
    list(setmat = set_matrix, getmat = get_matrix,
         setinv = setinv_matrix,
         getinv = getinv_matrix)
}


## cacheSolve function takes the matrix object x created by makeCacheMatrix function as its input,
## returns a matrix inverse from cache if it exists, if it does not exist, source matrix definition 
## is obtained via x$getmean object method call, then the inverted matrix is calculated by the solve 
## function(if the source matrix is invertible)  and its value is stored in the cache using x$setinv 
## object method call and finally its value is returned.   

cacheSolve <- function(x, ...) {
    ## the matrix inverse of 'x' is returned (if it exists) from cache  
    inv_m <- x$getinv() 
    ## if a matrix inverse exists in the cache its value is returned and function cacheSolve ends.
    if(!is.null(inv_m)) {
        message("getting cached data")
        return(inv_m)
    }
    ## otherwise a x$getmat object method is called and the source matrix value is returned  
    data <- x$getmat() 
    ## a matrix inverse is calculated using the R solve function
    inv_m <- solve(data, ...)
    ## and the value of the inverse is stored in the cache by invoking the setinv object method of the object
    x$setinv(inv_m)
    ## the matrix inverse is returned  
    inv_m    
}
