library(stringr)

count_repeated_pair <- function(text) {
    return(str_count(text, "(..).*\\1"))
}

count_double_sandwich <- function(text) {
    return(str_count(text, "(.).\\1"))
}

is_nice <- function(text) {
    if (count_repeated_pair(text) < 1) {
        return(FALSE)
    } else if (count_double_sandwich(text) < 1) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

test_cases <- c(
    "qjhvhtzxzqqjkmpb",
    "xxyxx",
    "uurcxstgmygtbstg",
    "ieodomkazucvgmuy"
)

for (tc in test_cases) {
    print(is_nice(tc))
}

file <- file("solutions/day5/data.txt", open = "r")
lines <- readLines(file)
close(file)

num_nice <- 0
for (l in lines) {
    if (is_nice(l)) {
        num_nice <- num_nice + 1
    }
}

print(num_nice)
