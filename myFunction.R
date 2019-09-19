function (roads, car, packages) 
{
  nextMove = 0
  toGo = 0
  offset = 0 
  
  if (car$load == 0) {
    toGo = which(packages[, 5] == 0)[1] # Says which package is the closest. 
    # OBS: a future implementation could be to not just take the closest package. BUT the package which has the lowest 
    # route cost. Then change this if loop and the below else. 
  }
  else {
    toGo = car$load # Otherwise gives the possition to the variable and sets an offset so the algorithm knows if it is 
    # a drop off or a pick up. 
    offset = 2  
  }
  
  # Calculate manhattan distance matrix: 
  calc_manhattan <- function(x,y,dim) {
    final_matrix <- matrix(rep(1,dim*dim),ncol=dim)
    for (i in 1:dim) {
      for (k in 1:dim) {
        my_pos <- c(x,y)
        current_pos <- c(i,k)
        final_matrix[i,k] <- dist(rbind(my_pos,current_pos), method = "manhattan")
      }
    }
    return(final_matrix)
  }
  
  manhattan_matrix <- calc_manhattan(car$x, car$y, dim(roads$hroads)[2])
  
  # Now implement my frontier: 
  
  
  if (car$x < packages[toGo, 1 + offset]) {
    nextMove = 6
  }
  else if (car$x > packages[toGo, 1 + offset]) {
    nextMove = 4
  }
  else if (car$y < packages[toGo, 2 + offset]) {
    nextMove = 8
  }
  else if (car$y > packages[toGo, 2 + offset]) {
    nextMove = 2
  }
  else {
    nextMove = 5
  }
  car$nextMove = nextMove
  car$mem = list()
  return(car)
}