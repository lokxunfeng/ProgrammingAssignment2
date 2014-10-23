## A pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL  ## Initial cached mean is NULL (no value)
        set <- function(y) {     ## resetting new matrix value, hence the mean should be reset to NULL and recalculated
                x <<- y
                m <<- NULL
        }
        get <- function() x   ## retrieve the matrix data
        setmean <- function(mean) m <<- mean  ## externally calculate the mean and set the mean value into the cache
        getmean <- function() m  ## retrieve the cached mean value
        list(set = set, get = get, ## return a list of functions. application e.g. test<- makeCacheMatrix(x); test$get()
             setmean = setmean,
             getmean = getmean)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { ## application e.g. cacheSolve(test)
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmean()   ## retrieving the cached mean by calling the above function. either get a value or NULL
        if(!is.null(m)) {  ## not NULL, then return the number calculated before (stored in cache)
                message("getting cached data")
                return(m) ## return the value, break off the function
        }
        data <- x$get() ## since mean is not calculated, retrieve the matrix data
        m <- mean(data, ...) ## calculate the mean
        x$setmean(m) ## record the calculated mean into the cache by calling above function
        m ## return and print out the mean calculated, which is just cached/updated into cache
}
