### Coursera R Programming Track Week 3 assignment 
## Calculating the inverse of a matrix can be a computionally expensive task
## especially when we start dealing with extremely large ones.
## By cacheing the inverse of the matirx rather than repeatidly computing it
## allows us to code smart and more efficently. 
## The following two functions are used to create an object than stores a matrix
## and caches it's inverse


## The first function is creating the object

makeCacheMatrix <- function(x = matrix()) {
        
        # here we will do the following
        # 1. set the matrix
        # 2. get the matrix
        # 3. set the inverse of the matrix
        # 4. get the inverse of the matrix
        
        inv <- NULL               # we initally set this to NULL but will change
                                  # when user sets the value
        set <- function(y) {
                x <<- y           #here we set the matrix but not the inverse
                inv <<- NULL
        }
        get <- function() x       # We get the matix but not the inverse 
        setinverse <- function(inverse) inv <<- inverse # mannually set inverse
        getinverse <- function() inv      #get the inverse
        # pull everything into a list
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)  
        
}


## once we have run makeCacheMatrix we have created our matrix and can use the
## function below to compute the inverse and cache the result.



cacheSolve <- function(x, ...) {
        # First check if the matrix has been computed yet
        inv <- x$getinverse()
        if(!is.null(inv)) {    # if it hass return the computed invese
                message("getting cached data.") #let user known it's being 
                                                #returned from cache
                return(inv)
        }
        #if it hasn't be computed then need to get the matrix and calculate it
        
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}



## in practice to use these function we would first define a martix
## x <- matrix(1:4, nrow =2, ncol =2)
## run this through makeCacheMatrix() and save the result
## m <- makeCacheMatrix(x)
## find the inverse using cacheSolve()
## s1 <- cacheSolve(m)
## if we run again we will resceive message "Getting cached data"
##s2 <- cacheSolve(m)
