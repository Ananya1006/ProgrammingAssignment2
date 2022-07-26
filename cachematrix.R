## Put comments here that give an overall description of what your
## functions do

## there are two functions makeCacheMatrix,
##it has set, get,setinv,getinv
##library(MASS) is used to calculate inverse of non squared and squared matrices

 library(MASS)
  makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
    set <- function(y){                   
                      x <<- y                             
                      inv <<- NULL                       
                      }
    get <- function()x                     ## define the get fucntion - returns value of the matrix argument
    setinv <- function(inverse)inv <<- inverse  
    getinv <- function(){
                        inver <-ginv(x)
                        inver%*%x               ##function to get inverse of the matrix
                        }                      
    list(set = set, get = get, 
         setinv = setinv,
         getinv = getinv) 
    }
  

## Write a short comment describing this function
##this is used to get cache data

cacheSolve <- function(x, ...) 
  {
  inv <- x$getinv()
  if(!is.null(inv)) {      #checking whether inverse is NULL
                    message("getting cached data!")
                    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv                        #return a matrix that is the inverse of x
}
