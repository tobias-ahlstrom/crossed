source("functions.R")

test <- create_crosswordmatrix(12, 9)

get_edge_indices(test, "up")
get_edge_indices(test, "right")
get_edge_indices(test, "down")
get_edge_indices(test, "left")


get_boundaries_of_index(test, 17)

is_connected_indices(test, 14, 17, "down")
is_connected_indices(test, 14, 17, "right")
is_connected_indices(test, 12, 108, "right")
is_connected_indices(test, 12, 107, "right")

count_adjacent(test, 15, "clue", "right")
count_adjacent(test, 29, "solution", "down")
count_adjacent(test, 61, "solution", "up")
count_adjacent(test, 100, "clue", "right")

is_valid_index(test, index = 27, rule = 1)
is_valid_index(test, index = 33, rule = 1)

is_valid_index(test, index = 26, rule = 3)
is_valid_index(test, index = 37, rule = 3)
is_valid_index(test, index = 12, rule = 3)
is_valid_index(test, index = 108, rule = 3)

get_indices_directional_of_index(test, 27, "up")
get_indices_directional_of_index(test, 27, "right")
get_indices_directional_of_index(test, 27, "down")
get_indices_directional_of_index(test, 27, "left")

for (i in seq(1, length(test))) {
  print(i)
  print(is_valid_index(test, index = i, rule = 1))
}

for (i in seq(1, length(test))) {
  print(i)
  print(is_valid_index(test, index = i, rule = 2))
}

adjust_matrix(test, 1)
adjust_matrix(test, 2)
adjust_matrix(test, 3)
