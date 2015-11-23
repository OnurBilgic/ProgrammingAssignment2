## 4 part set ,get ,setcache,getcache
 
## I'm getting x which is a square and inversible . Otherwise it can not work. 

makeCacheMatrix <- function(x = matrix()){
  m <- NULL ## m solve value 
  set <- function(y) {  ## cache values for x and m 
  x <<- y
  m <<- NULL
   }
   get <- function(){x} ## where x is stored
   setcache <- function(){ ## gives a inverse matrix to "m" value
     m <- solve(x)} 
   getcache <- function(){m} ## where m is stored
   list(set = set, get = get,   ##store all element easy to find
        setcache = setcache,
        getcache = getcache)
}



cacheSolve <- function(x) {
  m <<- solve(x$get())   ##give m value      
  if(!is.null(m)) {
    message("getting cached data") ## for sign 
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setcache(m)
  m
}
