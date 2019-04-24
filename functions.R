createcrosswordmatrix <- function(rows, cols) {
  
  clues_factor <- 3
  
  left_edge <- seq(1, rows)
  top_edge <- 1 + rows * seq(0, cols - 1)
  right_edge <- seq(rows * (cols - 1) + 1, rows * cols)
  bottom_edge <- rows + rows * seq(0, cols - 1)
  
  crossword_matrix <- matrix(0, nrow = rows, ncol = cols)
  
  n_clues_init <- as.integer((rows * cols)/clues_factor)
  
  # Perhaps adjust probabilities to set clues on specific rows/cols combination. 
  # Eg top row and left col could have more clues
  rand_clues_index <- sample(seq(1, rows * cols), size = n_clues_init, replace = FALSE)
  
  # -1 means down and 1 means right
  crossword_matrix[rand_clues_index] <- sample(c(-1, 1), size = n_clues_init, replace = TRUE)
  
  # Index [1, 1] must always be a clue
  crossword_matrix[1, 1] <- 1
  
  # Index [rows, cols] must always be a 0
  crossword_matrix[rows, cols] <- 0
  
  # Right edge must always be vertical and bottom edge must be horisontal
  crossword_matrix[right_edge] <- -1
  crossword_matrix[bottom_edge] <- 1
  
  # Validate the matrix, rule based:
  # 1. A clue can not have adjacent clues to the right and below, except if its the top left corner -> remove
  # 2. A clue's solution length must be greater than one -> flip direction. if right edge or bottom edge, remove
  # 3. An input cell must have a clue -> make the cell a clue, in the direction that gives the longest word
  # 4. Optional. All solutions must be connected -> If a solution is "isolated", remove its clue
  
  return(crossword_matrix)
}

test <- createcrosswordmatrix(12, 9)

# Needed functions
# count_adjacent(index, look_for, direction)
# get_solution_indices(index)
