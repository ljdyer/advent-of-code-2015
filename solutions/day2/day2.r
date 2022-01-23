amount_of_paper <- function(l, w, h) {
    side1 <- l * w
    side2 <- w * h
    side3 <- h * l
    surface_area <- 2 * side1 + 2 * side2 + 2 * side3
    slack <- min(side1, side2, side3)
    return (surface_area + slack)
}

amount_of_ribbon <- function(l, w, h) {
    sides_short_to_long <- sort(c(l, w, h))
    shortest <- sides_short_to_long[1]
    next_shortest <- sides_short_to_long[[2]][1]
    smallest_perimeter <- 2 * shortest + 2 * next_shortest
    volume <- l * w * h
    return(smallest_perimeter + volume)
}

file <- file("day2/data.txt", open = "r")
lines <- readLines(file)
close(file)

total_paper <- 0
total_ribbon <- 0

for (l in lines){
    dimensions <- unlist(strsplit(l, "x"))
    l <- as.integer((dimensions[[1]]))
    w <- as.integer((dimensions[[2]]))
    h <- as.integer((dimensions[[3]]))
    total_paper <- total_paper + amount_of_paper(l, w, h)
    total_ribbon <- total_ribbon + amount_of_ribbon(l, w, h)
}

print(total_paper)
print(total_ribbon)