library(readr)
library(stringr)
library(here)

exit <- function() {
    invokeRestart("abort")
}

my_data <- read_file("day1/data.txt")

# === Part One ===
print(str_count(my_data, "\\(") - str_count(my_data, "\\)"))

# === Part Two ===
chars <- unlist(strsplit(my_data, ""))

current_floor <- 0
counter <- 0

for (c in chars) {
    counter <- counter + 1
    if (c == "(") {
        current_floor <- current_floor + 1
    } else if (c == ")"){
        current_floor <- current_floor - 1
    }
    if (current_floor == "-1") {
        print(counter)
        exit()
    }
}
