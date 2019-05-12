source("functions.R")

# dictionary <- read_tsv("http://runeberg.org/words/ss100.txt", col_names = "word", col_types = "c")
# 
# Encoding(dictionary$word) <- "windows-1252"
# 
# dictionary %>% write_rds("dictionary.rds")

# Let dictionary be a global variable for now
dictionary <-  read_rds("dictionary.rds") %>% 
  mutate(word = str_remove_all(word, "[[:punct:]]"))

crossword_matrix <- create_crossword_matrix(12, 9, clues_factor = 5)

game_matrix <- create_game_matrix(crossword_matrix)

index_matrix <- matrix(seq(1, 12*9), nrow = 12, ncol = 9)


get_edge_indices(crossword_matrix, "up")
get_edge_indices(crossword_matrix, "right")
get_edge_indices(crossword_matrix, "down")
get_edge_indices(crossword_matrix, "left")

get_boundaries_of_index(crossword_matrix, 17)

is_connected_indices(crossword_matrix, 14, 17, "down")
is_connected_indices(crossword_matrix, 14, 17, "right")
is_connected_indices(crossword_matrix, 12, 108, "right")
is_connected_indices(crossword_matrix, 12, 107, "right")

count_adjacent(crossword_matrix, 15, "clue", "right")
count_adjacent(crossword_matrix, 29, "solution", "down")
count_adjacent(crossword_matrix, 61, "solution", "up")
count_adjacent(crossword_matrix, 100, "clue", "right")

is_valid_index(crossword_matrix, index = 27, rule = 1)
is_valid_index(crossword_matrix, index = 33, rule = 1)

is_valid_index(crossword_matrix, index = 26, rule = 3)
is_valid_index(crossword_matrix, index = 37, rule = 3)
is_valid_index(crossword_matrix, index = 12, rule = 3)
is_valid_index(crossword_matrix, index = 108, rule = 3)

is_valid_index(crossword_matrix, index = 38, rule = 1)
is_valid_index(crossword_matrix, index = 38, rule = 3, force = "solution")
is_valid_index(crossword_matrix, index = 38, rule = 4)

get_indices_directional_of_index(crossword_matrix, 27, "up")
get_indices_directional_of_index(crossword_matrix, 27, "right")
get_indices_directional_of_index(crossword_matrix, 27, "down")
get_indices_directional_of_index(crossword_matrix, 27, "left")

get_adjacent_indices(crossword_matrix, 1, "solution", "right")

adjust_matrix(crossword_matrix, 1)
adjust_matrix(crossword_matrix, 2)
adjust_matrix(crossword_matrix, 3)

game_suggest_solution(crossword_matrix, game_matrix, 1, sample_size = 5)
game_suggest_solution(crossword_matrix, game_matrix, 40, sample_size = 5)


game_insert_solution(crossword_matrix, game_matrix, 1, "cia") %>% 
  game_insert_solution(crossword_matrix, ., 40, "knackar") %>% 
  game_insert_solution(crossword_matrix, ., 17, "ektorp") %>% 
  game_suggest_solution(crossword_matrix, ., 6, sample_size = 5)

game_insert_solution(crossword_matrix, game_matrix, 1, "cia") %>% 
  game_insert_solution(crossword_matrix, ., 40, "knackar") %>% 
  game_insert_solution(crossword_matrix, ., 17, "ektorp") %>% 
  get_solution(crossword_matrix, ., 40)

game_insert_solution(crossword_matrix, game_matrix, 1, "cia") %>% 
  game_insert_solution(crossword_matrix, ., 40, "knackar") %>% 
  game_insert_solution(crossword_matrix, ., 17, "ektorp") %>% 
  is_finished_clue(crossword_matrix, ., c(17, 40, 2))

game_matrix_test <- game_matrix %>% 
  game_insert_solution(crossword_matrix, ., 40, game_suggest_solution(crossword_matrix, ., 40, sample_size = 5)[1]) %>% 
  game_insert_solution(crossword_matrix, ., 17, game_suggest_solution(crossword_matrix, ., 17, sample_size = 5)[1]) %>% 
  game_insert_solution(crossword_matrix, ., 6,  game_suggest_solution(crossword_matrix, ., 6,  sample_size = 5)[1]) %>% 
  game_insert_solution(crossword_matrix, ., 28, game_suggest_solution(crossword_matrix, ., 28, sample_size = 5)[1]) %>% 
  game_insert_solution(crossword_matrix, ., 50, game_suggest_solution(crossword_matrix, ., 50, sample_size = 5)[1]) %>% 
  game_insert_solution(crossword_matrix, ., 20, game_suggest_solution(crossword_matrix, ., 20, sample_size = 5)[1]) %>% 
  View("test")

get_clue_indices(crossword_matrix, 13)
get_clue_indices(crossword_matrix, 29)
get_clue_indices(crossword_matrix, c(29, 41, 53, 65, 77, 89))

get_solution_indices(crossword_matrix, 1) %>% 
  get_clue_indices(crossword_matrix, .) %>% 
  .[,3] %>% 
  as.vector() %>% 
  na.omit() %>% 
  .[!is_finished_clue(crossword_matrix, game_matrix, .)]


