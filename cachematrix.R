#This function makes the LIST that contains a matrix....
#Warning, it does nto create a regular matrix.
#The list created has to be this special format
#the cache solve function will NOT solve for a normal matrix
# a normal matrix is not accepeted to cachesolve
#you will also not be able to run Cache solve the first time without 
#running this function first

makeCacheMatrix <- function(x = numeric()) {

	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}



        ## Return a matrix that is the inverse of 'x'
#The function returns a matrix but it first has to check the list to
#see if a solution has already been cached

cacheSolve <- function(x,...) {
       
   m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m


}
