## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
#  This function creates a special "natrix" object that can cache its inverse
#  By Y. Zhang

#   Initilization  
        m<-NULL;     

#  1. set the value of the matrix  
        set <- function(y) {
                x <<- y;
                m <<- NULL;
        }
#  2. get the value of the matrix
        get <- function() x
        
#  3. set the value of the matrix Inverse      
        setMatrixInverse<-function(inverse) m <<- inverse
#  4.  get the value of the matrix Inverse
        getMatrixInverse<-function() m
        
#  5.  Return the "matrix" of function group
        
        list(set=set, get=get, 
             setMatrixInverse=setMatrixInverse, 
             getMatrixInverse=getMatrixInverse)
}

## Test Case

## test_m<-matrix(c(4,3,1,1),2,2)
## m = makeCacheMatrix(test_m)
## m$get()
##  [,1] [,2]
##  [1,]    4    1
##  [2,]    3    1

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
# This function calculate matrix inverse x or return a matrix that is the inverse of 'x' if it is cached
         m <- x$getMatrixInverse();
         if(! is.null(m)) { 
             message("get cached matrtix inverse") 
             return(m) 
         } 
         data<- x$get() 
         m <- solve(data) 
         message("calculate matrix inverse")
         x$setMatrixInverse(m) 
         m
}

## cacheSolve(m)
## calculate matrix inverse
##      [,1] [,2]
##[1,]    1   -1
##[2,]   -3    4

## cacheSolve(m)
## get cached matrtix inverse
##  i   [,1] [,2]
##  [1,]    1   -1
##  [2,]   -3    4
