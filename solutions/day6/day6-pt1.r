library(stringr)

get_nums <- function(instruction) {
    return(unlist(str_extract_all(instruction, "\\d+")))
}

file <- file("solutions/day6/data.txt", open = "r")
lines <- readLines(file)
close(file)

lights <- matrix(FALSE, nrow = 1000, ncol = 1000)

for (l in lines) {
    words <- unlist(strsplit(l, " "))
    range <- as.integer(get_nums(l))
    range <- range + 1
    if (words[[1]] == "toggle") {
        for (x in range[[1]]:range[[3]]) {
            for (y in range[[2]]:range[[4]]) {
                lights[x, y] <- !lights[x, y]
            }
        }
    } else if (words[[2]] == "on") {
        lights[range[[1]]:range[[3]], range[[2]]:range[[4]]] <- TRUE
    } else if (words[[2]] == "off") {
        lights[range[[1]]:range[[3]], range[[2]]:range[[4]]] <- FALSE
    }
}

print(sum(rowCounts(lights)))
