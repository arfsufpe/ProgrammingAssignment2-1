#makeCacheMatrix: This function creates a special "matrix" object 
# that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){
        
        #the inverse matrix
        i <- NULL
        
        #sets the matrix
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        #gets the matrix
        get <- function() x
        
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#cacheSolve: This function computes the inverse of the special 
#"matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated 
#(and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x,...){
        
        i <- x$getinverse()
        
        #return the inverse (if already calculated)
        if(!is.null(i)){
                message("getting the cached inverse matrix")
                return (i)
        }
        
        #get the matrix
        data <- x$get()
        
        #calculates the inverse
        i <- solve(data,...)
        
        #set the inverse the the cached matrix
        x$setinverse(i)
        
        #returns the inverse
        i
}

