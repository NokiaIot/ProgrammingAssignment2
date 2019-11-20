## Define a matrix and feed it into makeCacheMatrix()
## Call the cacheSolve() to get inverse
## In case you wish to find inverse of another matrix, invoke set()
## If new data set is same as the first, inverse is returned from cache 
## Else, it is recalculated and special vectors are reset

## Within makeCacheMatrix, set() checks if the former data is same as new
## As inverse is only possible for square matrices, only length of row is checked for the two matrices
## If they are different then obviously the matrices are different and inverse is calculated afresh
## If they are of equal dimension, the values are compared by subtracting the two matrices
## and sum of absolute values of individual elements calculated
## If sum of that is 0 it means the two data sets are equal and inv retains original value
## Else inv is set to null and inverse will be calculated again 
## get() returns the matrix when called
## setinv() sets the new inverse function after it is calculated
## getinv() returns the inverse as stored 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                if(nrow(x)!=nrow(y))
                {
                x <<- y
                inv <<- NULL
                } else {               
                 sub<-sum(abs(x-y))
                     if(sub!=0)
                     {
                     x <<- y
                     inv <<- NULL
                     }  
                 }
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve() first checks gets the inverse and checks if it has a value
## If it is not "null" then it prints "getting cached data" and returns inverse stored in cache, Exits
## Else, it obtains the data stored and calculates inverse using solve function
## The new value is stored in cache and it exits

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}


## To check the assignment, please try the following:
## Define a matrix- x<-matrix(c(5,7,6,8),2,2)
## Set a sim object- sim<-makeCacheMatrix(x)
## Call cacheSolve- op<-cacheSolve(sim)
## Check the output which is the inverse matrix
## Now try another input matrix- y<-matrix(c(3,2,6,8),2,2)
## Set the new value- sim$set(y)
