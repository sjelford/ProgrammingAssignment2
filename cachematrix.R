## The makeCacheMatrix creates a special "matrix" by assigning values 
## to a different environment

## makeCacheMatirx creates a special matrix "x"

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)

}


## cacheSolve returns the inverse matrix of "x" 
## but it checks to see if values already cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
