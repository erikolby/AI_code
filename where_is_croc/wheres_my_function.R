wheres_my_function <- function(moveInfo, readings, positions, edges, probs) {
  #moveInfo$moves = c(sample(getOptions(positions[3], edges), 1), 0)
  #return(moveInfo)
  move_ranger <- function(my_node, edges, goal_node) {
    frontier <- list()
    visited <- list()
    current_path <- list()
    frontier_and_visited <- list()
    return_value <- 0
    
    while (goal_node != my_node) {
      first_column_index <- which(edges[,1] == my_node)
      second_column_index <- which(edges[,2] == my_node)
      in_frontier <- sapply(frontier, '[[', 1)
      length_first_column <- length(first_column_index)
      
      if (length_first_column > 0) {
        for (i in 1:length(first_column_index)) {
          if (!(edges[first_column_index[i],2] %in% in_frontier)) {
            frontier <- append(frontier, list(list(position = edges[first_column_index[i], 2], 
                                      path = append(current_path, edges[first_column_index[i], 1]))))
          
          }
        }
      }
      
      in_visited <- sapply(visited, '[[', 1)
      length_second_column <- length(second_column_index)
      
      if (length_second_column > 0) {
        for (i in 1:length(second_column_index)) {
          if (!(edges[second_column_index[i], 1] %in% in_visited)) {
            frontier <- append(frontier, list(list(position = edges[second_column_index[i], 1], 
                                      path = append(current_path, edges[second_column_index[i], 2]))))
          }
        }
      }
      # The smallest node (depth first search)
      #smallest_index <- which.min(sapply(frontier, '[[', 1))
      next_node <- frontier[[1]] # next node (breadth first)
    
      # Removing the smallest index from the frontier. 
      frontier <- frontier[-1]
        
      # Adding the visited nodes. 
      visited <- append(visited, next_node) 
    
      # changing the currently expanded node. 
      my_node <- next_node[['position']]
      current_path <- next_node[['path']]
      if (length(current_path) > 1) {
      return_value <- current_path[[2]]
      }
      else {
        return_value <- my_node
      }
      }
      return(return_value)

  }
  a_value <- move_ranger(1, edges, 8)
  print(a_value)
  }
