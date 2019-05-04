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


# Must also consider that unfinished solutions will be possible to complete
game_suggest_solution <- function(crossword_matrix, game_matrix, index) {
  
  if (crossword_matrix[index] == 0) {
    stop(paste("Index", index, "is not a clue"))
  }
  
  solution_length <- count_adjacent(crossword_matrix, index, look_for = "solution", ifelse(crossword_matrix[index] == 1, "right", "down"))
  
  solution_indices <- get_adjacent_indices(crossword_matrix, index, look_for = "solution", ifelse(crossword_matrix[index] == 1, "right", "down"))
  
  current_solution <- game_matrix[solution_indices] %>% 
    str_c(collapse = "")
  
  dictionary_subset <- dictionary %>% 
    filter(str_length(word) == solution_length,
           str_detect(str_to_lower(word, locale = "sv"), current_solution))
  
  suggestions <- sample(dictionary_subset$word, 5)
  
  return(suggestions)
}



game_insert_solution <- function(crossword_matrix, game_matrix, index, solution) {
  
  solution_candidate <- str_to_lower(solution, locale = "sv")
  
  if (crossword_matrix[index] == 0) {
    stop(paste("Index", index, "is not a clue"))
  }
  
  solution_length <- count_adjacent(crossword_matrix, index, look_for = "solution", ifelse(crossword_matrix[index] == 1, "right", "down"))
  
  if (str_length(solution_candidate) != solution_length) {
    stop("Incorrect solution length")
  }
  
  solution_indices <- get_adjacent_indices(crossword_matrix, index, look_for = "solution", ifelse(crossword_matrix[index] == 1, "right", "down"))
  
  current_solution <- game_matrix[solution_indices] %>% 
    str_c(collapse = "")
  
  if (!str_detect(solution_candidate, current_solution)) {
    stop("Solution does not fit")
  }
  
  solution_letters <- str_split(solution_candidate, "", simplify = TRUE) %>% 
    as.vector()
  
  game_matrix[solution_indices] <- solution_letters
  
  return(game_matrix)
  
}



