library(stringr)

count_vowels <- function(text) {
    return (str_count(text, "[aeiou]"))
}

count_doubles <- function(text) {
    return(str_count(text, "(.)\\1"))
}

count_disallowed <- function(text) {
    return(str_count(text, "ab|cd|pq|xy"))
}

is_nice <- function(text) {
    if (count_vowels(text) < 3) {
        return(FALSE)
    } else if (count_doubles(text) < 1) {
        return(FALSE)
    } else if (count_disallowed(text) > 0) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

test_cases <- c(
    "ugknbfddgicrmopn",
    "aaa",
    "jchzalrnumimnmhp",
    "haegwjzuvuyypxyu",
    "dvszwmarrgswjxmb"
)

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