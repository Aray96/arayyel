#makeCacheMAtrix
makeCacheMatrix = function(x=matrix()){
    inv = NULL
    set = function(y){
      x<<-y
      inv<<- NULL
    }
    get = function() {x}
    setInverse = function(inverse) {inv<<-inverse}
    getInverse = function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
 
cacheSolve = function(x, ...){
  inv = x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
    mat = x$get()
    inv = solve(mat, ...)
    x$setInverse(inv)
    inv
}
 

#PollutantMean
pollutantmean = function (directory, pollutant, id= 1:332){
  mylist = list.files(path = directory, pattern = ".csv")
  x=numeric()
  for(i in id) {
    mydata = read.csv(mylist[i])
    x=c(x,mydata[[pollutant]])
  }
  mean(x, na.rm = T)
}

#complete
complete = function(directory, id = 1:332) {
  mylist = list.files(path = directory, pattern = ".csv")
  nobs = numeric()
  for (i in id) {
    mydata = read.csv(mylist[i])
    mysum = sum(complete.cases(mydata))
    nobs = c(nobs,mysum)
  }
  data.frame(id, nobs)
}


