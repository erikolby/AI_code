wheres_my_function <- function(moveInfo, readings, positions, edges, probs) {
  
  calc_transition_matrix <- function(edges) { 
    transition_matrix <- matrix(rep(0,40*40),ncol=40)
    for (i in 1:40) {
      # Picking out where the nodes edges and calculating the length of the index vectors. 
      first_column_index <- which(edges[,1] == i)
      second_column_index <- which(edges[,2] == i)
      length_first_column <- length(first_column_index)
      length_second_column <- length(second_column_index)
      neighbour <- 0
      # Only looking at the first column and length must be larger then 0. 
      if (length_first_column > 0) {
        # for all the found edges. 
        for (k in 1:length_first_column) {
          # The node connecting to our investigated is taken. 
          neighbour <- edges[first_column_index[k],2]
          # All of the edges that this node has is analyzed. 
          neighbour_index_one <- which(edges[,1] == neighbour)
          neighbour_index_two <- which(edges[,2] == neighbour)
          # The transition matrix is uppdated on the column of the originally investigated node
          # and the transition probability is put on the corresponding spot.  
          transition_matrix[neighbour, i] <- 1/(length(neighbour_index_one)+length(neighbour_index_two))  
        }
      }
      # Only looking at the first column and length must be larger then 0. 
      if (length_second_column > 0) {
        # for all the found edges. 
        for (k in 1:length_second_column) {
          # The node connecting to our investigated is taken. 
          neighbour <- edges[second_column_index[k],1]
          # All of the edges that this node has is analyzed. 
          neighbour_index_one <- which(edges[,1] == neighbour)
          neighbour_index_two <- which(edges[,2] == neighbour)
          # The transition matrix is uppdated on the column of the originally investigated node
          # and the transition probability is put on the corresponding spot.  
          transition_matrix[neighbour, i] <- 1/(length(neighbour_index_one)+length(neighbour_index_two))
        }
      }
          }
    return(transition_matrix)
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
    #print("----- Initializing -----")
    moveInfo$mem$move_matrix <- calc_move_matrix(edges)
    moveInfo$mem$probability_vector <- numeric(40)
    moveInfo$mem$trans_matrix <- calc_transition_matrix(edges) # Calculate the transition state matrix 
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
   
  if (moveInfo$mem$status == 2) {
    current_probs <- moveInfo$mem$probability_vector
    trans_matrix <- moveInfo$mem$trans_matrix 
    # Uppdating probability vector... 
    # Standard markow chain plus emissionprobs: 
    new_probs <- 0
    for (i in 1:40) {
      new_probs[i] <- current_probs%*%trans_matrix[,i]*(dnorm(readings[1], probs$salinity[i,1], probs$salinity[i,2])*
                                                          dnorm(readings[2], probs$phosphate[i,1], probs$phosphate[i,2])*
                                                          dnorm(readings[3], probs$nitrogen[i,1], probs$nitrogen[i,2]))
    }
    
    
    for (i in 1:2) {
      if (is.na(positions[i])) {
        # Here I should add some kind of cooldown factor for the dead backpacker.. If it recently happened, use it!
      }
      else if (positions[i] > 0) {
        new_probs[positions[i]] = 0
      } 
      else {
        new_probs[positions[i]] = 1
      }
    }
    
    # Normalizing the calculated vector. 
    new_pobs_sum <- sum(new_probs)
    for (i in 1:40) {
      new_probs[i] <- new_probs[i]/new_pobs_sum
    }
    original_winning_node <- which.max(new_probs)
    
    search_own_position <- 0 # If the current node is the one winning, search! 
    if (original_winning_node == positions[3]) {
      search_own_position = 1
    }
    
    # If conesting node is close, check it out. 
    contesting_nodes <- which(new_probs > new_probs[original_winning_node]/6) # is the current position always in contesting nodes? otherwise, if it is, could be good to search where you are. 
    close_by_first <- 0
    close_by_second <- 0
      first_column_index <- which(edges[,1] == positions[3])
      second_column_index <- which(edges[,2] == positions[3])
      length_first_column_index <- length(first_column_index)
      length_second_column_index <- length(second_column_index)
      
      if (length_first_column_index > 0) {
        for (i in 1:length_first_column_index) {
          close_by_first[i] <- edges[first_column_index[i],2]
        }
      }
      if (length_second_column_index > 0) {
        for (i in 1:length_second_column_index) {
          close_by_second[i] <- edges[second_column_index[i],1]
        }
      }
      winning_parameter <- 0
      winning_node <- 0
      for (i in 1:length(contesting_nodes)) {
      if (contesting_nodes[i] %in% close_by_first) {
        winning_node = contesting_nodes[i]
        winning_parameter <- 1
      }
      if (contesting_nodes[i] %in% close_by_second) {
        winning_node = contesting_nodes[i]
        winning_parameter <- 1
      }
      }
      # handling the first move: 
      move_1 <- moveInfo$mem$move_matrix[positions[3], winning_node]
      # How to handle the second move: 
      if (winning_parameter == 0) {
        move_1 <- moveInfo$mem$move_matrix[positions[3], original_winning_node]
        move_2 <- moveInfo$mem$move_matrix[move_1, original_winning_node]
      }
      else {
        move_2 <- 0
      }
      if (search_own_position == 1) {
        move_1 <- 0
      }
  }
  
  if (move_1 == 0) {
   new_probs[positions[3]] = 0
  }
  if (move_2 == 0) {
    new_probs[move_1] = 0
  }
  
  moveInfo$mem$probability_vector <- new_probs
  moveInfo$moves <- c(move_1, move_2)
  return(moveInfo)
  
  
  
  
  
  
  
  
  
  }
