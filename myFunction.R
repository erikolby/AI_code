myFunction <- function (roads, car, packages) 
{
  nextMove = 0
  toGo = 0
  offset = 0 
  
  # Dubble check my a*, my package thing gets stuck between values.. if the car focuses on one, the problem is solved.. 
  
  if (car$load == 0) {
    toGo = which(packages[, 5] == 0)
    distances <- sapply(toGo, function(item) dist(rbind(c(car$x, car$y), c(packages[item,1],packages[item,2])), method = "manhattan"))
    next_package <- which.min(distances)[1]
    toGo <- toGo[next_package]
    #print(toGo)
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
  
  # Write a function that checks if the node has already been passet by this node. If it has, do not add it to the frontier. 
  check_frontier <- function(x, y, current_visited_list) {
    if (length(current_visited_list) != 0) {
      visited_x <- sapply(current_visited_list, '[[', 1)
      visited_y <- sapply(current_visited_list, '[[', 2)
      visited_vector <- mapply(c, visited_x, visited_y)
      vector_of_logic <- sapply(1:dim(visited_vector)[2], function(item) identical(visited_vector[,item], c(x,y)))
      if (all(vector_of_logic == FALSE)) {
        return(TRUE)
      }
      else {
        return(FALSE)
      }
    }
    else {
      return(TRUE)
    }
  }
  
  
  # Calculate the manhattan distance to the package (or the delivery spot):
  a_star <- function(offset, toGo, roads, packages, car) {
    dimension <- dim(roads$hroads)[2]
    manhattan_matrix <- calc_manhattan(packages[toGo,1+offset], packages[toGo,2+offset], dimension)
    manhatt_dist <- manhattan_matrix[car$x,car$y]
    #print(manhatt_dist)
    goal_node <- c(packages[toGo,1+offset], packages[toGo,2+offset])
    frontier <- list()
    currently_expanded <- list(list(position = list(x = car$x, y = car$y), path_cost = 0, manhatt_dist = manhatt_dist, 
                                    visited_nodes = list()))
    test <- 0
    #goal_node <- c(packages[toGo,1+offset], packages[toGo,2+offset])
    if (car$x == goal_node[1] && car$y == goal_node[2]) {
      return(c(goal_node[1], goal_node[2]))
      #print("At the node!")
    }
    
    while (currently_expanded[[1]]$position$x != goal_node[1] || currently_expanded[[1]]$position$y != goal_node[2]) {
      
      x_pos <- currently_expanded[[1]]$position$x
      y_pos <- currently_expanded[[1]]$position$y
      #cat(sprintf("My x-pos: %d\n", x_pos))
      #cat(sprintf("My y-pos: %d\n", y_pos))
      current_path_cost <- currently_expanded[[1]]$path_cost
      #print(current_path_cost)
      current_visited_list <- currently_expanded[[1]]$visited_nodes
      #print(length(current_visited_list))
      test <- test+1
      print(test)
      #print(goal_node[1])
      #print(goal_node[2])
      #print(currently_expanded[[1]]$position$x)
      #print(currently_expanded[[1]]$position$y)
      x_blocked <- 0
      y_blocked <- 0
      #if (test > dimension*100) {
      #  return(c(goal_node[1], goal_node[2]))
      #}
      
      if (x_pos == 1) { 
        if (check_frontier(2, y_pos, current_visited_list)) {
        frontier <- append(frontier, list(list(position = list(x = 2, y = y_pos), 
                                              path_cost = current_path_cost+roads$hroads[1,y_pos], 
                                              manhatt_dist = manhattan_matrix[2, y_pos],
                                              visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
        
        }
        x_blocked <- 1
      }
      
      if (x_pos == dimension) {
        if (check_frontier(dimension-1, y_pos, current_visited_list)) {
        frontier <- append(frontier, list(list(position = list(x = dimension-1, y = y_pos), 
                                               path_cost = current_path_cost+roads$hroads[dimension-2,y_pos], 
                                               manhatt_dist = manhattan_matrix[dimension-1, y_pos],
                                               visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
        
        }
        x_blocked <- 1
      }
      
      if (y_pos == 1) {
        if (check_frontier(x_pos, 2, current_visited_list)) {
        frontier <- append(frontier, list(list(position = list(x = x_pos, y = 2), 
                                               path_cost = current_path_cost+roads$vroads[x_pos, 1], 
                                               manhatt_dist = manhattan_matrix[x_pos, 2],
                                               visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
        
        }
        y_blocked <- 1
      }
      
      if (y_pos == dimension) {
        if (check_frontier(x_pos, dimension-1, current_visited_list)) {
        frontier <- append(frontier, list(list(position = list(x = x_pos, y = dimension-1), 
                                               path_cost = current_path_cost+roads$vroads[x_pos, dimension-2], 
                                               manhatt_dist = manhattan_matrix[x_pos, dimension-1],
                                               visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
        
        }
        y_blocked <- 1
      }
      
      if (x_blocked == 1 && y_blocked != 1) {
        if (check_frontier(x_pos, y_pos+1, current_visited_list)) {
        frontier <- append(frontier, list(list(position = list(x = x_pos, y = y_pos+1), 
                                               path_cost = current_path_cost+roads$vroads[x_pos, y_pos], 
                                               manhatt_dist = manhattan_matrix[x_pos, y_pos+1],
                                               visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
        }
        
        if (check_frontier(x_pos, y_pos-1, current_visited_list)) {
        frontier <- append(frontier, list(list(position = list(x = x_pos, y = y_pos-1), 
                                               path_cost = current_path_cost+roads$vroads[x_pos, y_pos-1], 
                                               manhatt_dist = manhattan_matrix[x_pos, y_pos-1],
                                               visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
        }
      }
      
      if (x_blocked != 1 && y_blocked == 1) {
        if (check_frontier(x_pos+1, y_pos, current_visited_list)) {
        frontier <- append(frontier, list(list(position = list(x = x_pos+1, y = y_pos), 
                                               path_cost = current_path_cost+roads$hroads[x_pos, y_pos], 
                                               manhatt_dist = manhattan_matrix[x_pos+1, y_pos],
                                               visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
        }
        
        if (check_frontier(x_pos-1, y_pos, current_visited_list)) {
        frontier <- append(frontier, list(list(position = list(x = x_pos-1, y = y_pos), 
                                               path_cost = current_path_cost+roads$hroads[x_pos-1, y_pos], 
                                               manhatt_dist = manhattan_matrix[x_pos-1, y_pos],
                                               visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
        }
      }
      
      if (x_blocked == 0 && y_blocked == 0) {
        if (check_frontier(x_pos+1, y_pos, current_visited_list)) {
        frontier <- append(frontier, list(list(position = list(x = x_pos+1, y = y_pos), 
                                               path_cost = current_path_cost+roads$hroads[x_pos, y_pos], 
                                               manhatt_dist = manhattan_matrix[x_pos+1, y_pos],
                                               visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
        }
        
        if (check_frontier(x_pos-1, y_pos, current_visited_list)) {
        frontier <- append(frontier, list(list(position = list(x = x_pos-1, y = y_pos), 
                                               path_cost = current_path_cost+roads$hroads[x_pos-1, y_pos], 
                                               manhatt_dist = manhattan_matrix[x_pos-1, y_pos],
                                               visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
        }
        
        if (check_frontier(x_pos, y_pos+1, current_visited_list)) {
        frontier <- append(frontier, list(list(position = list(x = x_pos, y = y_pos+1), 
                                               path_cost = current_path_cost+roads$vroads[x_pos, y_pos], 
                                               manhatt_dist = manhattan_matrix[x_pos, y_pos+1],
                                               visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
        }
        
        if (check_frontier(x_pos, y_pos-1, current_visited_list)) {
        frontier <- append(frontier, list(list(position = list(x = x_pos, y = y_pos-1), 
                                               path_cost = current_path_cost+roads$vroads[x_pos, y_pos-1], 
                                               manhatt_dist = manhattan_matrix[x_pos, y_pos-1],
                                               visited_nodes = append(current_visited_list, list(c(x_pos, y_pos))))))
        }
        
      }
      
      scores=sapply(frontier, function(item) item$path_cost+item$manhatt_dist)
      best_index=which.min(scores)
      currently_expanded <- currently_expanded[-1]
      currently_expanded <- append(currently_expanded, frontier[best_index])
      frontier <- frontier[-best_index]
    }
    if (length(currently_expanded[[1]]$visited_nodes) == 1) {
      currently_expanded[[1]]$visited_nodes <- append(currently_expanded[[1]]$visited_nodes, list(c(goal_node[1], goal_node[2])))
      #print("next to the node!")
      }
    #if (currently_expanded[[1]]$position$x == goal_node[1] && currently_expanded[[1]]$position$y == goal_node[2]) {
    #  print(2)
    #  return(c(goal_node[1],goal_node[2]))
    #}
    #print(sapply(currently_expanded, function(item) item$visited_nodes))
    
    
    new_direction <- currently_expanded[[1]]$visited_nodes[[2]]
    # Den av någon anledning lägger bara in ett objekt i visited_nodes, vilket gör att det blir out of bounds? a* kommer
    # fram till att den borde stanna på platsen??
    return(new_direction)
    
  } 
  
  new_direction <- a_star(offset, toGo, roads, packages, car)
  #print(new_direction)
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