function (roads, car, packages) 
{
  nextMove = 0
  toGo = 0
  offset = 0 
  
  if (car$load == 0) {
    toGo = which(packages[, 5] == 0)[1] # Says which package is the closest. (apperently not, change this to implement the 
    # manhattan distance).
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
  
  # Calculate the manhattan distance to the package (or the delivery spot):
  if (offset == 0) {
    manhattan_matrix <- calc_manhattan(packages[toGo,1], packages[toGo,2], dim(roads$hroads)[2])
    manhatt_dist <- manhattan_matrix[car$x,car$y]
    # Now implement my first frontier and expanded node: 
    
    frontier <- list(list(position = list(x = car$x, y = car$y), path_cost = 0, manhatt_dist = manhatt_dist, 
                              visited_nodes = list()))
    currently_expanded <- frontier[[1]]
    
    # Gör snarare frontier till bara en list() och kör ovan frontier definition till currently_expended. 
    
    #defining the goal node: 
    goal_node <- c(packages[toGo,1], packages[toGo,2])
    
    while (currently_expanded[[1]]$position$x != goal_node[1] && currently_expanded[[1]]$position$y != goal_node[2]) {
      # lös hörngrejerna på något sätt! 
      
      if (currently_expanded[[1]]$position$x == 1 && currently_expanded[[1]]$position$y == 1) { # Can remove this offset (it is 0)
        #nextMove = 6
        right_road <- currently_expanded[[1]]$position$x
        frontier <- append(frontier,list(list(position = list(x = 2, y = 1), 
                                              path_cost = currently_expanded[[1]]$path_cost+roads$hroads[1,1], 
                                              manahtt_dist = manhattan_matrix[2,1],
                                              visited_nodes = append(currently_expanded,)))
      }
      y = roads$vroads[1,1]
      
      
      
      scores=sapply(frontier, function(item) item$path_cost+item$manhatt_dist)
      best_index=which.min(scores)
      currently_expanded <- currently_expanded[-1]
      currently_expanded <- append(currently_expanded, frontier[best_index])
      # Not sure if the above name thing works. Test this. Also left to do: 
      # define the borders of the graphs, which are conected to which etc. We need to have some kind of updating 
      # of the frontier in some way! 
      # Do not name the nodelists! they can be nameless and called by eg. frontier[[1]]$scores.
      # The appending needs to be done: frontier <- append(frontier, list("My_list_of_four"))
    }
    
  } 
  else {
    manhatt_dist <- manhattan_matrix[packages[toGo,3],packages[toGo,4]]
  }
  
  
  
  

  
  
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