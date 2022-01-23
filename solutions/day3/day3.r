library(readr)
library(stringr)

move <- function(coord, direction) {
    x <- coord[[1]]
    y <- coord[[2]]
    new_coord <- switch(
        direction,
        "<" = c(x - 1, y),
        ">" = c(x + 1, y),
        "^" = c(x, y + 1),
        "v" = c(x, y - 1)
    )
    return(new_coord)
}

my_data <- read_file("day3/data.txt")
directions <- unlist(strsplit(my_data, ""))

# === Part One ===

current_pos <- c(0, 0)
visited <- list(current_pos)

for (d in directions) {
    current_pos <- move(current_pos, d)
    visited[[length(visited) + 1]] <- c(current_pos)
}

print(length(unique(visited)))

# === Part Two ===

santa_pos <- c(0, 0)
robo_santa_pos <- c(0, 0)
visited <- list(santa_pos)
santa_next <- TRUE

for (d in directions) {
    if (santa_next) {
        santa_pos <- move(santa_pos, d)
        visited[[length(visited) + 1]] <- c(santa_pos)
    } else {
        robo_santa_pos <- move(robo_santa_pos, d)
        visited[[length(visited) + 1]] <- c(robo_santa_pos)
    }
    santa_next <- !santa_next
}

print(length(unique(visited)))