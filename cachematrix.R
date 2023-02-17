##################################################
#EXAMPLE WITH MEAN
#makeVector <- function(x = numeric()) {
#        m <- NULL
#        set <- function(y) {
#                x <<- y
#                m <<- NULL
#        }
#        get <- function() x
#        setmean <- function(mean) m <<- mean
#        getmean <- function() m
#        list(set = set, get = get,
#             setmean = setmean,
#             getmean = getmean)
#}

#cachemean <- function(x, ...) {
#        m <- x$getmean()
#        if(!is.null(m)) {
#                message("getting cached data")
#                return(m)
#        }
#        data <- x$get()
#        m <- mean(data, ...)
#        x$setmean(m)
#        m
#}
##################################################

#MY CODE

#create a matrix 
mat <- matrix(c(9,7,3,1,2,4,8,5,6),3,3)
mat

#check the inverse to compare with my functions
try<- solve(mat)
try


###makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

test<-makeCacheMatrix(mat)
test$get()



###cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x' (test)
cacheSolve <- function(x, ...) {
  
  test <- x$getinv()
  
  #if cacheSolve as already been execute, write a message in the console
  if(!is.null(test)) {
    message("getting cached data")
    return(test)
  }
  
  #calculate the inverse of the matrix cache previously
  data <- x$get()
  test <- solve(data, ...)
  
  #sets the value of the inverse in the cache via the setinv function.
  x$setinv(test)
  test
}

cacheSolve(test)
#try a second time to get the message
cacheSolve(test)

#Test the inverse to find the initial matrix
mat <- matrix(c(-0.1038961,-0.3506494,0.2857143,0.3376623,0.3896104,-0.4285714,-0.1428571,0.1428571,0.1428571),3,3)
test<-makeCacheMatrix(mat)
test$get()
cacheSolve(test)
