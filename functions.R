create_crossword_matrix <- function(rows, cols, clues_factor = NULL) {
  
  if (is.null(clues_factor)) {
    clues_factor <- 3
  }
  
  up_edge <- 1 + rows * seq(0, cols - 1)
  right_edge <- seq(rows * (cols - 1) + 1, rows * cols)
  down_edge <- rows + rows * seq(0, cols - 1)
  left_edge <- seq(1, rows)
  
  crossword_matrix <- matrix(0, nrow = rows, ncol = cols)
  
  n_clues_init <- as.integer((rows * cols)/clues_factor)
  
  # Perhaps adjust probabilities to set clues on specific rows/cols combination. 
  # Eg top row and left col could have more clues
  
  set.seed(100)
  rand_clues_index <- sample(seq(1, rows * cols), size = n_clues_init, replace = FALSE)
  
  # -1 means down and 1 means right
  set.seed(101)
  crossword_matrix[rand_clues_index] <- sample(c(-1, 1), size = n_clues_init, replace = TRUE)
  
  # Some simple checks
  # Index [1, 1] must always be a clue
  crossword_matrix[1, 1] <- 1
  
  # Index [rows, cols] must always be a 0
  crossword_matrix[rows, cols] <- 0
  
  # Right edge must always be vertical clues and bottom edge must be horisontal clues
  crossword_matrix[intersect(which(crossword_matrix == 1), right_edge)] <- -1
  crossword_matrix[intersect(which(crossword_matrix == -1), down_edge)] <- 1
  
  # Index [1, 2] and [2, 1] can not both be clues
  if (get_edge_indices(crossword_matrix, "left")[2] != 0 & get_edge_indices(crossword_matrix, "up")[2] != 0) {
    # Pick a random of these and set to 0
    crossword_matrix[sample(c(get_edge_indices(crossword_matrix, "left")[2], get_edge_indices(crossword_matrix, "up")[2]), size = 1)] <- 0
  }
  
  # Validate the matrix, rule based:
  # 1. A clue can not have adjacent clues to the right and below, except if its the top left corner -> remove clue
  # 2. A clue's solution length must be greater or equal to one -> flip direction. if right edge or down edge, remove
  # 3. An input cell must have a clue -> make the cell a clue, in the direction that gives the longest word
  # TODO: New clue from rule 3 can be moved back further, either up or left
  # 4. A cell first violates 1 and if adjusted also violates 3 -> Remove both clues to right and down
  # 5. Optional. All solutions must be connected -> If a solution is "isolated", remove its clue
  
  # Perhaps not validate through looping through in order, rather go through all indices in random order
  
  is_valid_crossword <- FALSE
  pass <- 1
  
  while (!is_valid_crossword) {
    
    cat("Pass", pass, "\n")
    
    check_1 <- adjust_matrix(crossword_matrix, 1)
    crossword_matrix <- check_1$crossword_matrix
    
    check_2 <- adjust_matrix(crossword_matrix, 2)
    crossword_matrix <- check_2$crossword_matrix
    
    check_3 <- adjust_matrix(crossword_matrix, 3)
    crossword_matrix <- check_3$crossword_matrix
    
    check_4 <- adjust_matrix(crossword_matrix, 4)
    crossword_matrix <- check_4$crossword_matrix
    
    cat("Rule 1 adjustments:", check_1$adjustments, "\n")
    cat("Rule 2 adjustments:", check_2$adjustments, "\n")
    cat("Rule 3 adjustments:", check_3$adjustments, "\n")
    cat("Rule 4 adjustments:", check_4$adjustments, "\n")
    cat("\n")
    
    pass <- pass + 1
    
    if (check_1$adjustments + check_2$adjustments + check_3$adjustments + check_4$adjustments == 0) {
      is_valid_crossword <- TRUE
    }
    
    print(crossword_matrix)
    
  }
  
  return(crossword_matrix)
}



create_game_matrix <- function(crossword_matrix) {
  game_matrix <- matrix(".", nrow = nrow(crossword_matrix), ncol = ncol(crossword_matrix))
  
  clues_right_indices <- which(crossword_matrix == 1)
  clues_down_indices <- which(crossword_matrix == -1)
  
  game_matrix[clues_right_indices] <- "c_r"
  game_matrix[clues_down_indices] <- "c_d"
  
  return(game_matrix)
}


# Rewrite to call the get function and return length of that
count_adjacent <- function(crossword_matrix, index, look_for, direction) {
  step <- case_when(
    direction == "up" ~ as.integer(-1),
    direction == "right" ~ nrow(crossword_matrix),
    direction == "down" ~ as.integer(1),
    direction == "left" ~ -nrow(crossword_matrix)
  )
  
  if (look_for == "clue") {
    look_for_value <- c(1, -1)
  } else if (look_for == "solution") {
    look_for_value <- 0
  }
  
  next_index = index + step
  pre_index = index
  count <- 0
  
  while (next_index <= length(crossword_matrix) & next_index > 0 & is_connected_indices(crossword_matrix, pre_index, next_index, direction)) {
    if (crossword_matrix[next_index] %in% look_for_value) {
      count <- count + 1
      pre_index <- next_index
      next_index <- next_index + step
    } else {
      break()
    }
  }
  
  return(count)
}



get_adjacent_indices <- function(crossword_matrix, index, look_for, direction) {
  step <- case_when(
    direction == "up" ~ as.integer(-1),
    direction == "right" ~ nrow(crossword_matrix),
    direction == "down" ~ as.integer(1),
    direction == "left" ~ -nrow(crossword_matrix)
  )
  
  if (look_for == "clue") {
    look_for_value <- c(1, -1)
  } else if (look_for == "solution") {
    look_for_value <- 0
  }
  
  next_index = index + step
  pre_index = index
  indices <- c()
  
  while (next_index <= length(crossword_matrix) & next_index > 0 & is_connected_indices(crossword_matrix, pre_index, next_index, direction)) {
    if (crossword_matrix[next_index] %in% look_for_value) {
      indices <- c(indices, next_index)
      pre_index <- next_index
      next_index <- next_index + step
    } else {
      break()
    }
  }
  
  return(indices)
}



is_connected_indices <- function(crossword_matrix, start_index, end_index, direction) {
  
  if (start_index == end_index) {
    return(TRUE)
  }
  
  boundaries <- get_boundaries_of_index(crossword_matrix, start_index)
  
  if (direction == "up") {
    
    if (end_index < start_index & end_index >= boundaries$up) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    
  } else if (direction == "right") {
    
    if (end_index > start_index & start_index %% nrow(crossword_matrix) == end_index %% nrow(crossword_matrix)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    
  } else if (direction == "down") {
    
    if (end_index > start_index & end_index <= boundaries$down) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    
  } else if (direction == "left") {
    
    if (end_index < start_index & start_index %% nrow(crossword_matrix) == end_index %% nrow(crossword_matrix)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    
  } else {
    indices <- NULL
  }
}



get_boundaries_of_index <- function(crossword_matrix, index) {
  
  up_edge <- get_edge_indices(crossword_matrix, "up")
  down_edge <- get_edge_indices(crossword_matrix, "down")
  
  match_vec <- (index >= up_edge) & (index <= down_edge)
  
  boundary.up <- up_edge[match_vec]
  boundary.down <- down_edge[match_vec]
  
  
  right_edge <- get_edge_indices(crossword_matrix, "right")
  left_edge <- get_edge_indices(crossword_matrix, "left")
  
  row_of_index <- ifelse(index %% nrow(crossword_matrix) > 0, index %% nrow(crossword_matrix), nrow(crossword_matrix))
  
  boundary.right <- right_edge[row_of_index]
  boundary.left <- left_edge[row_of_index]
  
  boundaries <- list(up = boundary.up, right = boundary.right, down = boundary.down, left = boundary.left)
  
  return(boundaries)
  
}



get_indices_directional_of_index <- function(crossword_matrix, index, direction) {
  
  boundaries <- get_boundaries_of_index(crossword_matrix, index)
  
  if (direction == "up" & !(index %in% get_edge_indices(crossword_matrix, "up"))) {
    
    out <- seq(boundaries$up, index - 1) %>% 
      sort(decreasing = TRUE)
    
  } else if (direction == "right" & !(index %in% get_edge_indices(crossword_matrix, "right"))) {
    
    out <- seq(index + nrow(crossword_matrix), boundaries$right, by = nrow(crossword_matrix))
    
  } else if (direction == "down" & !(index %in% get_edge_indices(crossword_matrix, "down"))) {
    
    out <- seq(index + 1, boundaries$down)
    
  } else if (direction == "left" & !(index %in% get_edge_indices(crossword_matrix, "left"))) {
    
    out <- seq(boundaries$left, index - nrow(crossword_matrix), by = nrow(crossword_matrix)) %>% 
      sort(decreasing = TRUE)
    
  } else {
    out <- NULL
  }
  
  return(out)
}



get_edge_indices <- function(crossword_matrix, side) {
  
  rows <- nrow(crossword_matrix)
  cols <- ncol(crossword_matrix)
  
  if (side == "up") {
    indices <- 1 + rows * seq(0, cols - 1)
  } else if (side == "right") {
    indices <- seq(rows * (cols - 1) + 1, rows * cols)
  } else if (side == "down") {
    indices <- rows + rows * seq(0, cols - 1)
  } else if (side == "left") {
    indices <- seq(1, rows)
  } else {
    indices <- NULL
  }
  
  return(indices)
}


# Validate the matrix, rule based:
# 1. A clue can not have adjacent clues to the right and below, except if its the top left corner -> remove clue
# 2. A clue's solution length must be greater or equal to one -> flip direction. if right edge or down edge, remove
# 3. An input cell must have a clue -> make the cell a clue, in the direction that gives the longest word
# TODO: New clue from rule 3 can be moved back further, either up or left
# 4. A cell first violates 1 and if adjusted also violates 3 -> Remove both clues to right and down
# 5. Optional. All solutions must be connected -> If a solution is "isolated", remove its clue

# Perhaps not validate through looping through in order, rather go through all indices in random order
adjust_matrix <- function(crossword_matrix, rule) {
  
  adjustments <- 0
  
  if (rule == 1) {
    
    set.seed(102)
    indices <- sample(seq(2, length(crossword_matrix)), size = (length(crossword_matrix) - 1), replace = FALSE)
    
    for (i in indices) {
      
      if (!is_valid_index(crossword_matrix, i, 1)) {
        crossword_matrix[i] <- 0
        adjustments <- adjustments + 1
      }
    }
    
  } else if (rule == 2) {
    
    set.seed(103)
    indices <- sample(seq(1, length(crossword_matrix)), size = length(crossword_matrix), replace = FALSE)
    
    for (i in indices) {
      
      if (!is_valid_index(crossword_matrix, i, 2)) {
        crossword_matrix[i] <- crossword_matrix[i] * -1
        if (i %in% get_edge_indices(crossword_matrix, "right") | i %in% get_edge_indices(crossword_matrix, "down")) {
          crossword_matrix[i] <- 0
        }
        adjustments <- adjustments + 1
      }
    }
    
  } else if (rule == 3) {
    
    set.seed(104)
    indices <- sample(seq(2, length(crossword_matrix)), size = (length(crossword_matrix) - 1), replace = FALSE)
    
    for (i in indices) {
      
      if (!is_valid_index(crossword_matrix, i, 3)) {
        if (count_adjacent(crossword_matrix, i, "solution", "right") > count_adjacent(crossword_matrix, i, "solution", "down")) {
          crossword_matrix[i] <- 1
        } else {
          crossword_matrix[i] <- -1
        }
        
        adjustments <- adjustments + 1
      }
    }
  } else if (rule == 4) {
    
    set.seed(105)
    indices <- sample(seq(1, length(crossword_matrix)), size = length(crossword_matrix), replace = FALSE)
    
    for (i in indices) {
      
      if (!is_valid_index(crossword_matrix, i, 4)) {
        
        crossword_matrix[get_indices_directional_of_index(crossword_matrix, i, "right")[1]] <- 0
        crossword_matrix[get_indices_directional_of_index(crossword_matrix, i, "down")[1]] <- 0
        
        adjustments <- adjustments + 1
      }
    }
  }
  
  return(list("crossword_matrix" = crossword_matrix, "adjustments" = adjustments))
}



is_valid_index <- function(crossword_matrix, index, rule, force = NULL) {
  
  if (!is.null(force)) {
    if (force == "clue_right") {
      crossword_matrix[index] <-  1
    } else if (force == "clue_down") {
      crossword_matrix[index] <-  -1
    } else if (force == "solution") {
      crossword_matrix[index] <-  0
    } else {
      stop("force needs to be one of clue_right, clue_down, solution or not specified")
    }
  }
  
  if (rule == 1) {
    
    if (crossword_matrix[index] == 0) {
      return(TRUE)
    }
    
    # Look right
    if (!(index %in% get_edge_indices(crossword_matrix, "right"))) {
      if (crossword_matrix[index + nrow(crossword_matrix)] == 0) {
        return(TRUE)
      }
    }
    
    # Look down
    if (!(index %in% get_edge_indices(crossword_matrix, "down"))) {
      if (crossword_matrix[index + 1] == 0) {
        return(TRUE)
      }
    }
    
    cat("Index", index, "violates rule 1\n")
    
    return(FALSE)
    
  } else if (rule == 2) {
    
    if (crossword_matrix[index] == 0) {
      return(TRUE)
    }
    
    if (count_adjacent(crossword_matrix, index, "solution", "right") > 0 & crossword_matrix[index] == 1) {
      return(TRUE)
    }
    
    if (count_adjacent(crossword_matrix, index, "solution", "down") > 0 & crossword_matrix[index] == -1) {
      return(TRUE)
    }
    
    cat("Index", index, "violates rule 2\n")
    
    return(FALSE)
    
  } else if (rule ==  3) {
    
    if (crossword_matrix[index] != 0) {
      return(TRUE)
    }
    
    # Look left
    left_indices <- get_indices_directional_of_index(crossword_matrix, index, "left")
    
    if (!is.null(left_indices)) {
      for (i in left_indices) {
        if (crossword_matrix[i] == -1) {
          break()
        } else if (crossword_matrix[i] == 1) {
          return(TRUE)
        }
      }
    }
    
    # Look up
    up_indices <- get_indices_directional_of_index(crossword_matrix, index, "up")
    
    if (!is.null(up_indices)) {
      for (i in up_indices) {
        if (crossword_matrix[i] == 1) {
          break()
        } else if (crossword_matrix[i] == -1) {
          return(TRUE)
        }
      }
    }
    
    cat("Index", index, "violates rule 3\n")
    
    return(FALSE)
    
  } else if (rule ==  4) {
    
    if (crossword_matrix[index] == 0) {
      return(TRUE)
    }
    
    if (!is_valid_index(crossword_matrix, index, 1) & !is_valid_index(crossword_matrix, index, 3, force = "solution")) {
      
      cat("Index", index, "violates rule 4\n")
      
      return(FALSE)
    }
    
    return(TRUE)
    
  }
}


# Give an index as .omit_clue to not check validity with that clue
game_suggest_solution <- function(crossword_matrix, game_matrix, index, sample_size = NULL, .omit_clue = NULL, .omit_suggestions = NULL) {
  
  if (crossword_matrix[index] == 0) {
    stop(paste("Index", index, "is not a clue"))
  }
  
  cat(index, "Find suggestion for index:", index, "\n")
  
  # Get current solution and subset dictionary with what fits based on that
  current_solution <- get_solution(crossword_matrix, game_matrix, index)
  
  cat(index, "Current text is:", current_solution, "\n")
  
  dictionary_subset <- dictionary %>% 
    filter(str_length(word) == str_length(current_solution),
           str_detect(str_to_lower(word, locale = "sv"), current_solution),
           !(word %in% .omit_suggestions)) %>% 
    pull(word)
  
  if (is_finished_clue(crossword_matrix, game_matrix, index)) {
    
    if (length(dictionary_subset) == 0) {
      cat(index, "Is a finished clue, but not a real word, return NULL\n\n")
      return(NULL)
    }
    
    cat(index, "Is a finished clue, return current text\n\n")
    return(current_solution)
  }
  
  dictionary_subset <- sample(dictionary_subset, min(max(sample_size, 10), length(dictionary_subset)))
  
  cat(index, "Dictionary subset is:\n")
  print(head(dictionary_subset, n = 10))
  
  if (length(dictionary_subset) == 0) {
    cat(index, "No available suggestions, return NULL\n\n")
    return(NULL)
  }
  
  
  # The rest of the code is to consider that unfinished solutions connected to the suggestion will be able to complete
  solution_indices <- get_solution_indices(crossword_matrix, index)
  
  connected_clues_indices <- get_clue_indices(crossword_matrix, solution_indices)
  
  # Only consider vertical clues if horizontal suggestion and vice versa
  if (crossword_matrix[index] == 1 & !all(is.na(connected_clues_indices[,3]))) {
    connected_clues_indices_unfinished <- connected_clues_indices[,3] %>% 
      as.vector() %>% 
      na.omit() %>% 
      .[!is_finished_clue(crossword_matrix, game_matrix, .)]
    
    # not_check_positions <- which(!(connected_clues_indices[,3] %in% connected_clues_indices_unfinished) & !is.na(connected_clues_indices[,3]))
    
  } else if (crossword_matrix[index] == -1 & !all(is.na(connected_clues_indices[,2]))) {
    connected_clues_indices_unfinished <- connected_clues_indices[,2] %>% 
      as.vector() %>% 
      na.omit() %>% 
      .[!is_finished_clue(crossword_matrix, game_matrix, .)]
    
    # not_check_positions <- which(!(connected_clues_indices[,2] %in% connected_clues_indices_unfinished) & !is.na(connected_clues_indices[,2]))
    
  } else {
    connected_clues_indices_unfinished <- NULL
  }
  
  # Used to specify which connected clues to recheck
  # Currently drills down only one step
  if (!is.null(.omit_clue)) {
    # connected_clues_indices_unfinished <- connected_clues_indices_unfinished[connected_clues_indices_unfinished != .omit_clue]
    connected_clues_indices_unfinished <- NULL
  }
  
  # Which positions in the word does not need to be checked
  # Currently does not work very well, rethink this
  
  # dictionary_subset_temp <- dictionary_subset %>% 
  #   str_split(pattern = "", simplify = TRUE)
  
  # dictionary_subset_temp[, not_check_positions] <- "."
  
  # dictionary_subset_temp <- apply(dictionary_subset_temp, 1, str_c, collapse = "") %>% 
  #   unique()
  
  dictionary_subset_possible <- c()
  nr_clues_to_satisfy <- length(connected_clues_indices_unfinished)
  
  # If there are no connected clues to satisfy, set possible clues to dictionary_subset
  if (nr_clues_to_satisfy == 0) {
    
    dictionary_subset_possible <- dictionary_subset
    cat(index, "No connected clues to check\n")
  } else {
    
    for (candidate_suggestion in dictionary_subset) {
      
      cat(index, "Candidate suggestion:", candidate_suggestion, "\n")
      
      game_matrix_temp <- game_insert_solution(crossword_matrix, game_matrix, index, candidate_suggestion, force = FALSE)
      
      n_satisfied <- 0
      
      for (unfinished_clue in connected_clues_indices_unfinished) {
        
        cat(index, "Drill down on clue:", unfinished_clue, "\n\n")
        
        # Enough to check that one single word satifies connected unfinished clue
        # Pass it's own index to not check it in further steps
        if (length(game_suggest_solution(crossword_matrix, game_matrix_temp, unfinished_clue, 1, .omit_clue = index)) > 0) {
          
          cat(index, "Can satisfy clue:", unfinished_clue, "\n")
          n_satisfied <- n_satisfied + 1
          
        } else {
          
          # If one single unfinished clue can not be satisfied, just break loop
          cat(index, "Can not satisfy clue:", unfinished_clue, "\n")
          break()
        }
      }
      
      if (n_satisfied == nr_clues_to_satisfy) {
        dictionary_subset_possible <- c(dictionary_subset_possible, candidate_suggestion)
      }
      
      # Found enough possible words, break
      if (length(dictionary_subset_possible) == sample_size) {
        break()
      }
    }
  }
  
  if (!is.null(sample_size)) {
    suggestions <- sample(dictionary_subset_possible, min(sample_size, length(dictionary_subset_possible)))
  } else {
    suggestions <- dictionary_subset_possible
  }
  
  # If no suggestions was found, try again
  # Pass dictionary_subset as suggestions not to re check
  if (length(suggestions) == 0) {
    suggestions <- game_suggest_solution(crossword_matrix, game_matrix, index, sample_size = sample_size, .omit_suggestions = dictionary_subset)
  }
  
  cat(index, "Return possible solutions:", paste0("[", suggestions, "]", collapse = ","), "for index,", index, "\n\n")
  return(suggestions)
}



game_insert_solution <- function(crossword_matrix, game_matrix, index, solution, force = FALSE) {
  
  solution_candidate <- str_to_lower(solution, locale = "sv")
  
  if (crossword_matrix[index] == 0) {
    stop(paste("Index", index, "is not a clue"))
  }
  
  solution_indices <- get_solution_indices(crossword_matrix, index)
  
  if (str_length(solution_candidate) != length(solution_indices)) {
    stop("Incorrect solution length")
  }
  
  current_solution <- get_solution(crossword_matrix, game_matrix, index)
  
  if (!str_detect(solution_candidate, current_solution) & !force) {
    stop("Solution does not fit")
  }
  
  solution_letters <- str_split(solution_candidate, "", simplify = TRUE) %>% 
    as.vector()
  
  game_matrix[solution_indices] <- solution_letters
  
  return(game_matrix)
  
}



is_finished_clue <- function(crossword_matrix, game_matrix, indices){
  
  if (any(crossword_matrix[indices] == 0)) {
    stop("All indices are not clues")
  }
  
  is_finished_clue_vector <- c()
  
  for (i in indices) {
    solution <- get_solution(crossword_matrix, game_matrix, i)
    
    if (str_count(solution, "\\.") == 0) {
      is_finished_clue_vector <- c(is_finished_clue_vector, TRUE)
    } else {
      is_finished_clue_vector <- c(is_finished_clue_vector, FALSE)
    }
  }
  
  return(is_finished_clue_vector)
}



get_solution <- function(crossword_matrix, game_matrix, index) {
  
  if (crossword_matrix[index] == 0) {
    stop(paste("Index", index, "is not a clue"))
  }
  
  solution_indices <- get_solution_indices(crossword_matrix, index)
  
  current_solution <- game_matrix[solution_indices] %>% 
    str_c(collapse = "")
  
  return(current_solution)
  
}



get_solution_indices <- function(crossword_matrix, index) {
  
  if (crossword_matrix[index] == 0) {
    stop(paste("Index", index, "is not a clue"))
  }
  
  solution_indices <- get_adjacent_indices(crossword_matrix, index, look_for = "solution", ifelse(crossword_matrix[index] == 1, "right", "down"))
  
  return(solution_indices)
  
}


get_clue_indices <- function(crossword_matrix, indices) {
  
  clue_indices <- matrix(nrow = length(indices), ncol = 3, byrow = TRUE, dimnames = list(seq_along(indices), c("solution_index", "left_clue_index", "up_clue_index")))
  
  if (any(crossword_matrix[indices] != 0)) {
    stop("All indices are not solutions")
  }
  
  row <- 1
  
  # Loop through all indices
  for (i in indices) {
    
    clue_indices[row, 1] <- i
    
    # Look left
    suppressWarnings(
      {
        leftmost_solution_index <- get_adjacent_indices(crossword_matrix, i, "solution", "left") %>% 
          min()
      }
    )
    
    if (is.infinite(leftmost_solution_index)) {
      index_to_check <- i
    } else {
      index_to_check <- leftmost_solution_index
    }
    
    if (index_to_check %in% get_edge_indices(crossword_matrix, "left")) {
      
      clue_indices[row, 2] <- NA
      
    } else if (crossword_matrix[index_to_check - nrow(crossword_matrix)] == -1) {
      
      clue_indices[row, 2] <- NA
      
    } else {
      
      clue_indices[row, 2] <- index_to_check - nrow(crossword_matrix)
      
    }
    
    # Look up
    suppressWarnings(
      {
        upmost_solution_index <- get_adjacent_indices(crossword_matrix, i, "solution", "up") %>% 
          min()
      }
    )
    
    if (is.infinite(upmost_solution_index)) {
      index_to_check <- i
    } else {
      index_to_check <- upmost_solution_index
    }
    
    if (index_to_check %in% get_edge_indices(crossword_matrix, "up")) {
      
      clue_indices[row, 3] <- NA
      
    } else if (crossword_matrix[index_to_check - 1] == 1) {
      
      clue_indices[row, 3] <- NA
      
    } else {
      
      clue_indices[row, 3] <- index_to_check - 1
      
    }
    
    row <- row + 1
  }
  
  return(clue_indices)
}
