## makeCacheMatrix takes a matrix as input and returns a list of functions
## that can help set and get the values from matrix and the cache i.e
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(A = matrix()) {
        ##initialize inverse matrix A to NULL
        invA <- NULL
        ##create a set new matrix & clear inverse of previous matrix from cache 
        set <- function(y){
                A <<- y
                invA <<- NULL
        }
        ##create a get function that returns back the input matrix 
        get <- function() A
        ##create a set function to set the cache value        
        setInv <- function(inv) invA<<-inv
        ##create a get function that returns the value in Cache 
        getInv <- function() invA
        ##create the return matrix
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve would 
## retrieve the inverse from the cache.

cacheSolve <- function(A, ...) {
        ##get the cache value into a local variable
        invA <- A$getInv()
        ##if cache not empty, then return the value from cache
        if(!is.null(invA)){
                message("getting cached data")
                return(invA)
        }
        ##if cache empty, then get the matrix data
        data <- A$get()
        ##calculate inverse matrix
        invA <- solve(data)
        ##set the value in cache
        A$setInv(invA)
        ##return the calculated value
        invA
}
