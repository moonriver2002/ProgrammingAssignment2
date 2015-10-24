## This is for the homework2 for Courera rprog-033 for matrix cache function.
## Created by Yongqing Zhang zhangyongqing2005@gmail.com

makeCacheMatrix <- function(x = matrix()) {
#  This function creates a special "natrix" object that can cache its inverse of a matrix
#  By Y. Zhang, 2015, 10.24

#   Initilization  of a inverse matrix data structure
        m<-NULL;     

#  1. set the value of the operation matrix , copy from Dr. Peng's Example 
        set <- function(y) {
                x <<- y;
                m <<- NULL;
        }
#  2. get the value of the matrix, also copy from Dr. Peng's example
        get <- function() x
        
#  3. set the value of the matrix inverse  calulated by using inverse function   
        setMatrixInverse<-function(inverse) m <<- inverse
#  4.  get the cached value of the inversed matrix
        getMatrixInverse<-function() m
#  5.  Return the "matrix" of cuttent function group
        
        list(set=set, get=get, 
             setMatrixInverse=setMatrixInverse, 
             getMatrixInverse=getMatrixInverse)
}

## Test Case: 

## test_m<-matrix(c(4,3,1,1),2,2)
## m = makeCacheMatrix(test_m)
## m$get()
##  [,1] [,2]
##  [1,]    4    1
##  [2,]    3    1

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
# This function calculate matrix inverse x or return a matrix inverse value if the x if that 'x' has been cached
         m <- x$getMatrixInverse();

#  If the matrix x inversed value is realdy in current scope, bring it out.
         if(! is.null(m)) { 
             message("get cached matrtix inverse") 
             return(m) 
         } 
         
#  If the inverse value is not in cache, calculate it.
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
