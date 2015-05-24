

# R Programming Assignment 2


## The purpose of this assignment is to demonstrate the lexical scoping
## characteristics in R through two functions 
## makeCacheMatrix and cacheSolve.
## The combination of these functions will result 
## in a matrix inverse that can be called any where in the session with no 
## computation costs. 

## Example: Rather than calculation 2+2 over and over, just save the number 4
## to be called at any point in the session


# makeCacheMatrix

## 1.) The inv, (inverse object is set to null) 
## later this will be the inverse matrix

## 2.) The set and get work together for 
## calculating the inverse of the matrix

## 3.) The final result is a list of the function inputs for
## the inverse calculation "getinverse"

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) 
  inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve

## 1.) Use the "getinverse" function to grab the inverse matrix
## 2.) if the inverse object is empty then go get the matrix and cacl the inverse
## 3.) if the inv object is not NULL, then the function will go get the cached
##     inverse calculated maxtrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

# Demo

#cat("\014") 

#y <- rbind(c(1, 2), c(2, 1))  
#print(y)

#y_v2 = makeCacheMatrix(y)
#cacheSolve(y_v2)
#cacheSolve(y_v2)


