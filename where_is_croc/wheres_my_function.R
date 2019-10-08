wheres_my_function <- function(moveInfo, readings, positions, edges, probs) {
  
  calc_transition_matrix <- function(edges) { 
    transition_matrix <- matrix(rep(0,40*40),ncol=40)
    for (i in 1:40) {
      first_column_index <- which(edges[,1] == i)
      second_column_index <- which(edges[,2] == i)
      length_first_column <- length(first_column_index)
      length_second_column <- length(second_column_index)
      #for (j in 1:40) {
      neighbour <- 0
      if (length_first_column > 0) {
        for (k in 1:length_first_column) {
          neighbour <- edges[first_column_index[k],2]
          neighbour_index_one <- which(edges[,1] == neighbour)
          neighbour_index_two <- which(edges[,2] == neighbour)
          transition_matrix[neighbour, i] <- 1/(length(neighbour_index_one)+length(neighbour_index_two))  
        #if (edges[j,1] == i) {
        #  first_column_neighbours = which(edges[,1] == edges[j,2])
        #  second_column_neighbours = which(edges[,2] == edges[j,2])
        #  transition_matrix[j,i]          
        }  
        # Find the transition probability from neighbour: 
      #  if (length_first_column > 0) {
       #   for (k in 1:length_first_column) {
        #    neighbours[k] <- edges[k,2]
            #output_transition_vector[j,i] <- (length(first_column_index)+length(second_column_index))/40
      #  }
       # }
        #if (length_second_column > 0) {
         # for (k in second_column_index) {
          #}
          }
      }
    }
  
    
  closest_path <- function(my_node, edges, goal_node) {
    frontier <- list()
    visited <- list()
    current_path <- list()
    frontier_and_visited <- list()
    return_value <- my_node
    
    while (goal_node != my_node) {
      first_column_index <- which(edges[,1] == my_node)
      second_column_index <- which(edges[,2] == my_node)
      in_frontier <- sapply(frontier, '[[', 1)
      length_first_column <- length(first_column_index)
      
      if (length_first_column > 0) {
        for (i in 1:length_first_column) { 
          if (!(edges[first_column_index[i],2] %in% in_frontier)) {
            frontier <- append(frontier, list(list(position = edges[first_column_index[i], 2], 
                                      path = append(current_path, edges[first_column_index[i], 1]))))
          
          }
        }
      }
      
      in_visited <- sapply(visited, '[[', 1)
      length_second_column <- length(second_column_index)
      
      if (length_second_column > 0) {
        for (i in 1:length_second_column) {
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
  
  calc_move_matrix <- function(edges) {
    move_matrix <- matrix(rep(1,40*40),ncol=40)
    for (i in 1:40) {
      for (j in 1:40) {
        move_matrix[i,j] <- closest_path(i, edges, j)
      }
    }
    return(move_matrix)
  }
  
  
  # When it is the first turn, calculate the move_matrix and add it to the mem field. 
  if (!(is.matrix(moveInfo$mem$move_matrix))) {
    print("Calculating matrix...")
    moveInfo$mem$move_matrix <- calc_move_matrix(edges)
    moveInfo$mem$probability_vector <- numeric(40)
  }
  
  # Create fresh probability vector if the status field is indicating a new game. (The first or any other).
  if (moveInfo$mem$status == 0 || moveInfo$mem$status == 1) {
    for (i in 1:40) {
      if (i %in% positions) {
        moveInfo$mem$probability_vector[i] <- 0
      }
      else {
        moveInfo$mem$probability_vector[i] <- 1/37
      }
    }
    moveInfo$mem$status = 2
  }
   
  
  
  random_walker <- function(moveInfo, positions) {
  the_goal <- sample(40,1)
  move_1 <- moveInfo$mem$move_matrix[positions[3], the_goal]
  move_2 <- sample(c(moveInfo$mem$move_matrix[move_1, the_goal],0),1)
  moveInfo$moves <- c(move_1, move_2)
  return(moveInfo)
  }
  
  # If we want a random walker... 
  return(random_walker(moveInfo, positions))
  
  
  
  
  
  
  
  
  
  }
