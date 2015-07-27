makeCacheMatrix <- function(x = matrix()) {
        ## create and return list of functions        
        
        ## store cached value
        ## assign NULL value
        inv <- NULL
        
        ## create matrix
        set <- function(y) {
                
                ## assign value to x & inv outside of current environment
                x <<- y
                inv <<- NULL
        }
        
        ## get value of matrix
        get <- function() x
        
        ## invert matrix, store value
        setinv <- function(inverse) inv <<- inverse
        
        
        ## get inverted matrix
        getinv <- function() inv
        
        ## return functions to working environment
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## calculate inverse of the matrix
cacheSolve <- function(x, ...)  {
        
        ## get inverted matrix from cache or create matrix in working
        ## environment if it doesn't exist
        inv = x$getinv()
        
        ## if the inverse has already been calculated, display message and
        ## return matrix
        if (!is.null(inv))  {
                message("getting cached data")
                return(inv)
        }
        
        ## if inverse not already calculated, calculate inverse now
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        ## set value of inverse in cache
        x$setinv(inv)
        
        return(inv)
}
