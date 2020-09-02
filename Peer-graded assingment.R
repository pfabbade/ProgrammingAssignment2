## INFO ----
## Title: Week 3 Peer-graded assingment
## Name: Pedro F. Abbade
## Date: 01-09-2020

## Assignment: Caching the Inverse of a Matrix ----

## Creating a matrix ----
  ram <- 1:4
  m <-matrix(data = ram, nrow = 2, ncol = 2)
  print(m) #matrix1
  
  ram2 <- rnorm(4,2,0.5)
  m2 <- matrix(ram2, 2, 2)
  print(m2) #matrix2
  
  ram3 <- rnorm(4,3,0.6)
  m3 <- matrix(ram3, 2, 2)
  print(m3) #matrix3

##  Function 1: creates a special "matrix" object that can cache its inverse ----
  
  makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
      x <<- y
      s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  }

## Function 2: computes the inverse of the special "matrix" returned by makeCacheMatrix function ----

## Attention:  If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

  cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
      message("getting inversed matrix")
      return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
  }
  
## Testing to see if both functions are working
  
cacheSolve(makeCacheMatrix(m)) #working properly
cacheSolve(makeCacheMatrix(m2)) #working properly
cacheSolve(makeCacheMatrix(m3)) #working properly
