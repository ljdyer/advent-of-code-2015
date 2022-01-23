library(matrixStats)
library(stringr)

exit <- function() {
    invokeRestart("abort")
}

get_nums <- function(instruction) {
    return(unlist(str_extract_all(instruction, "\\d+")))
}

file <- file("solutions/day6/data.txt", open = "r")
lines <- readLines(file)
close(file)

lights <- matrix(0, nrow = 1000, ncol = 1000)

for (l in lines) {
    words <- unlist(strsplit(l, " "))
    range <- as.integer(get_nums(l))
    range <- range + 1
    if (words[[1]] == "toggle") {
        increase_amt <- 2
    } else if (words[[2]] == "on") {
        increase_amt <- 1
    } else if (words[[2]] == "off") {
        increase_amt <- -1
    }
    for (x in range[[1]]:range[[3]]) {
        for (y in range[[2]]:range[[4]]) {
            lights[x, y] <- max(lights[x, y] + increase_amt, 0)

        }
    }
}

print(sum(rowSums(lights)))