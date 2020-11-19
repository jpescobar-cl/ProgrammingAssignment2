# This function generates a matrix, sets it's value and define the inverse matrix as NULL, before its calculated

makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL
    set <- function(y) {
        x <<- y
        m_inv <<- NULL
    }
    get <- function() {x}
    setinverse <- function(inverse) {m_inv <<- inverse}
    getinverse <- function() {m_inv}
    list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}

# This function firstly checks if the matrix exists and if its inverse has been calculated. After that, if its calculated 
# it obtains the matrix's inverse from the cache and skip the calculation process. If not, calculates the inverse and sets
# the value of the inverse in the cache through cachesolve()

cacheSolve <- function(x, ...) {
    m_inv <- x$getinverse()
    if(!is.null(m_inv)) {
        message("getting cached matrix data")
        return(m_inv)
    }
    mat <- x$get()
    m_inv <- solve(mat, ...)
    x$setinverse(m_inv)
    m_inv
}  ## Return a matrix that is the inverse of 'x'
