##When combined, these two functions will cache inverted matrices.

##makeCacheMatrix() will store data for a given matrix

makeCacheMatrix <- function(x = matrix()) { 
        ##'im' is just an empty inverse matrix variable to be overwritten
        im <- NULL                       
       
        ##set() will set value of matrix 'x' and empty out its stored inverse 
        ##by entering new matrix data
        set <- function(y) {            
                x <<- y                 
                im <<- NULL              
        }
        ##get() returns the value of 'x'
        get <- function() x             

        ##setinverse() sets 'im' as the inverse, done through the solve()
        ##function of cacheSolve() or through manual manipulation.
        setinverse <- function(inverse) im <<- inverse
                                        
        ##getinverse() returns the value of 'im'
        getinverse <- function() im      
        
        ##the function then stores a list of set(), get(), setinverse(), getinverse()
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}                                       

##cacheSolve() inverts a cached matrix if no inverse data have already been stored. 
##You can fool the function by overriding data using set() and setinverse().
##If you do so, then cacheSolve() will return the previously stored data, without actually attempting solve().

cacheSolve <- function(x, ...) {

        ##The inverse variable is set to matrix x's inverse by x's own getinverse().
        ##'im' will be "NULL" if there isn't a previously stored value
        im <- x$getinverse()

        ##If there is already a cached value for 'im' (other than NULL), then 
        ##return a message to notify that the value for 'im' was already stored 
        ##and then return that value.
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }

        ##If there is not a cached value for 'im', determine the inverse of the 
        ##matrix 'x' and store it as 'im'. Then return 'im'.
        data <- x$get()
        im <- solve(data, ...)
        x$setinverse(im)
        im
}
