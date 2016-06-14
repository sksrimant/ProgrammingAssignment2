#######################################################################################

# This function does the following
#a. Sets a Matrix
#b. Gets a Matrix
#c. Sets an Inverse Matrix
#d. Gets an Inverse Matrix.

#########################################################################################

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        
		set <- function(z) {
                x <<- z
                inv <<- NULL
        }
        get <- function() x
        
		setInverse <- function(inverse) inv <<- inverse			
        
		getInverse <- function() inv
        
		list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}


#######################################################################################

# This function does the following
#a. Picks the inverse from cache.
#b. Calculates the inverse if it is not in cache.

#########################################################################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat_value <- x$get()
        inv <- solve(mat_value,...)
        x$setInverse(inv)
        inv

		}
