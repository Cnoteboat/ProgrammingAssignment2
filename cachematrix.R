#This is my solution for programming Assignment 2

makeCacheMatrix<- function(x= matrix()){ #Here I define that we must be working with Matrices
    s <- NULL
    set <- function(y){  #Setting the Value of the Matrix
      x<<- y
      s<<- NULL
    }
    get<- function() x #Getting the Value of the Matrix
    setinverse<- function(solve) s <<- solve #Setting the value of the Inverse with the solve() function
    getinverse<- function() s #getting the Value of the inverse Matrix
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}

cacheSolve<- function(x, ...) {
  s <- x$getinverse() #get solved inverse Matrix
  if(!is.null(s)) {
    message("getting cached data") #check for cached solution where it returns "getting cached data" if the solution is cached
    return(s)
  }
  data <- x$get() #otherwise get the value of the matrix x
  s <- solve(data, ...) #and return the solution of matrix x
  x$setinverse(s) #
  s
}

