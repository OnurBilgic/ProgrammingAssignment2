## 4 part set ,get ,setcache,getcache


makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
  x <<- y
  m <<- NULL
   }
   get <- function(){x}
   setcache <- function(){
     m <- solve(x)}
   getcache <- function(){m} 
   list(set = set, get = get,
        setcache = setcache,
        getcache = getcache)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getcache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setcache(m)
  m
}
