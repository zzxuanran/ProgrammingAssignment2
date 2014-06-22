## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## initialize inverse matrix
        inv <- NULL
        
        ## set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## get the matrix
        get <- function() x
        
        ## set the inverse matrix
        setinverse <- function(inverse) inv <<- inverse
        
        ## get the inverse matrix
        getinverse <- function() inv
        
        ## return value 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## try to get the cached inverse matrix
        inv <- x$getinverse()
        
        ## if the inverse matrix exists, just return it
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## otherwise calculate the inverse matrix via solve() funxtion
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        
        ## return value
        inv
        
}


## ==========================
##      Testing Step
## ========================== 


## ---------- Construct matrix ----------
# > a<-matrix(c(1,1,0,0,1,0,0,0,1),3,3)
# > a
# [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    1    1    0
# [3,]    0    0    1

# > z<-makeCacheMatrix(a)

## ---------- Calculate the inverse matrix ----------
# > cacheSolve(z)
# [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]   -1    1    0
# [3,]    0    0    1


## ---------- Assign inverse matrix ----------
# > inv<-matrix(c(1,0,0,-1,1,0,0,0,1),3,3)
# > z$setinverse(inv)

## ---------- Get cached inverse matrix ----------
# > cacheSolve(z)
## getting cached data
# [,1] [,2] [,3]
# [1,]    1   -1    0
# [2,]    0    1    0
# [3,]    0    0    1
