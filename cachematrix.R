## makecache function contains four functions.It get and set the values of matrix and inverse of matrix
## in the parent environment
##cachesolve function first check whether given argument is calculated before or not.
##If the values are calculated before then inverse of matrix is retrieved from the parent environment
##If the values are not calculated then inverse of that matrix is calculated in the cachesolve function.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL                       #Initializing the value of m and x
  set<-function(y){             #Set the value of x as y in the parent environment
    x<<- y
    m<<- NULL                   #When another matrix value is given to the x and m value should be reset
  }
  get<-function() x             #Function for Getting the value of x from the parent environment
  setinver<-function(invers1) m <<- invers1     # m is defined in the parent environment,the value of m is assigned with the input value in this function
  getinver<-function() m   #Function for getting the value of m from parent environment
  list(set =set,get=get,setinver=setinver,getinver=getinver)  #Each of the function is given a name and defined within a list.It is returned to the parent environment.
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinver()  #m is assigned with the inverse value of matix from parent environment in x by calling the function
  if (!is.null(m)) { #To check whether cached value for the above matrix x is NULL or not
      message("getting cached inverse")  #If the cached value is present then it will print above message and get the value from the parent environment
      return(m)
  }
  data<-x$get() #To get the value of x from the parent environment
  m<-solve(data,...) #solve function is used to get the inverse value of new data and store value in m variable 
  x$setinver(m)  #To set the value of matrix in parent environment by calling the function
  m #Implicit printing of inverse of the matrix x
}


