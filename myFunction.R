myFunction <- function (roads, car, packages) 
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
  a_star <- function(offset, toGo, roads, packages, car) {
    dimension <- dim(roads$hroads)[2]
    manhattan_matrix <- calc_manhattan(packages[toGo,1+offset], packages[toGo,2+offset], dimension)
    manhatt_dist <- manhattan_matrix[car$x,car$y]
    
    frontier <- list()
    currently_expanded <- list(list(position = list(x = car$x, y = car$y), path_cost = 0, manhatt_dist = manhatt_dist, 
                                    visited_nodes = list()))
    
    goal_node <- c(packages[toGo,1], packages[toGo,2])
    
    while (currently_expanded[[1]]$position$x != goal_node[1] && currently_expanded[[1]]$position$y != goal_node[2]) {
      x_pos <- currently_expanded[[1]]$position$x
      y_pos <- currently_expanded[[1]]$position$y
      current_path_cost <- currently_expanded[[1]]$path_cost
      current_visited_list <- currently_expanded[[1]]$visited_nodes
      
      x_blocked <- 0
      y_blocked <- 0
      
      if (x_pos == 1) { 
        
        frontier <- append(frontier, list(list(position = list(x = 2, y = y_pos), 
                                              path_cost = current_path_cost+roads$hroads[1,y_pos], 
                                              manhatt_dist = manhattan_matrix[2, y_pos],
                                              visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
        x_blocked <- 1
      }
      
      if (x_pos == dimension-1) {
        frontier <- append(frontier, list(list(position = list(x = dimension-2, y = y_pos), 
                                               path_cost = current_path_cost+roads$hroads[dimension-2,y_pos], 
                                               manhatt_dist = manhattan_matrix[dimension-2, y_pos],
                                               visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
        x_blocked <- 1
        # TRUBBEL: Mathias har typ x_pos < 1 gör detta blabla tror jag. Om man inte kommer på något smart: 
        # forsätt på x_block och y_block, om den bara är x-block: ge y-led frihet, om den bara är y-blockat ge
        # x-led frihet, om både och, gör INGENTING, för dessa loopar har redan tagit hand om det! 
      }
      
      if (y_pos == 1) {
        frontier <- append(frontier, list(list(position = list(x = x_pos, y = 2), 
                                               path_cost = current_path_cost+roads$vroads[x_pos, 1], 
                                               manhatt_dist = manhattan_matrix[2, y_pos],
                                               visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
        y_blocked <- 1
      }
      
      if (y_pos == dimension-1) {
        frontier <- append(frontier, list(list(position = list(x = x_pos, y = dimension-2), 
                                               path_cost = current_path_cost+roads$vroads[x_pos, dimension-2], 
                                               manhatt_dist = manhattan_matrix[x_pos, dimension-2],
                                               visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
        y_blocked <- 1
      }
      
      if (x_blocked == 1 && y_blocked != 1) {
        frontier <- append(frontier, list(list(position = list(x = x_pos, y = y_pos+1), 
                                               path_cost = current_path_cost+roads$vroads[x_pos, y_pos], 
                                               manhatt_dist = manhattan_matrix[x_pos, y_pos+1],
                                               visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
        frontier <- append(frontier, list(list(position = list(x = x_pos, y = y_pos-1), 
                                               path_cost = current_path_cost+roads$vroads[x_pos, y_pos-1], 
                                               manhatt_dist = manhattan_matrix[x_pos, y_pos-1],
                                               visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
      }
      
      if (x_blocked != 1 && y_blocked == 1) {
        frontier <- append(frontier, list(list(position = list(x = x_pos+1, y = y_pos), 
                                               path_cost = current_path_cost+roads$vroads[x_pos, y_pos], 
                                               manhatt_dist = manhattan_matrix[x_pos+1, y_pos],
                                               visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
        frontier <- append(frontier, list(list(position = list(x = x_pos-1, y = y_pos), 
                                               path_cost = current_path_cost+roads$vroads[x_pos-1, y_pos], 
                                               manhatt_dist = manhattan_matrix[x_pos-1, y_pos],
                                               visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
      }
      
      if (x_blocked == 0 && y_blocked == 0) {
        frontier <- append(frontier, list(list(position = list(x = x_pos+1, y = y_pos), 
                                               path_cost = current_path_cost+roads$vroads[x_pos, y_pos], 
                                               manhatt_dist = manhattan_matrix[x_pos+1, y_pos],
                                               visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
        frontier <- append(frontier, list(list(position = list(x = x_pos-1, y = y_pos), 
                                               path_cost = current_path_cost+roads$vroads[x_pos-1, y_pos], 
                                               manhatt_dist = manhattan_matrix[x_pos-1, y_pos],
                                               visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
        frontier <- append(frontier, list(list(position = list(x = x_pos, y = y_pos+1), 
                                               path_cost = current_path_cost+roads$vroads[x_pos, y_pos], 
                                               manhatt_dist = manhattan_matrix[x_pos, y_pos+1],
                                               visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
        frontier <- append(frontier, list(list(position = list(x = x_pos, y = y_pos-1), 
                                               path_cost = current_path_cost+roads$vroads[x_pos, y_pos-1], 
                                               manhatt_dist = manhattan_matrix[x_pos, y_pos-1],
                                               visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
        
      }
      
      scores=sapply(frontier, function(item) item$path_cost+item$manhatt_dist)
      best_index=which.min(scores)
      currently_expanded <- currently_expanded[-1]
      currently_expanded <- append(currently_expanded, frontier[best_index])
    }
    
    new_direction <- currently_expanded[[1]]$visited_nodes[[1]]
    return(new_direction)
    
  } 
  
  new_direction <- a_star(offset, toGo, roads, packages, car)
  
  if (car$x < new_direction[1]) {
    nextMove = 6
  }
  else if (car$x > new_direction[1]) {
    nextMove = 4
  }
  else if (car$y < new_direction[2]) {
    nextMove = 8
  }
  else if (car$y > new_direction[2]) {
    nextMove = 2
  }
  else {
    nextMove = 5
  }
  car$nextMove = nextMove
  car$mem = list()
  return(car)
}