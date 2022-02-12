library(purrr)

wires <- list()
backlog <- list()

# ====================

exit <- function() {
    invokeRestart("abort")
}

int_to_bin <- function(a) {
    bin <- rep(0, times = 16)
    for (power in 15:0) {
        if (a >= 2 ^ power) {
            bin[[16 - power]] <- 1
            a <- a - (2 ^ power)
        }
    }
    return(bin)
}

bin_to_int <- function(a) {
    a_int <- 0
    for (index in 1:16) {
        if (a[[index]] == 1) {
            a_int <- a_int + (2 ^ (16 - index))
        }
    }
    return(a_int)
}

negate_bin_val <- function(x) {
    if (x == 0) {
        return(1)
    } else {
        return(0)
    }
}

bitwise_not <- function(a) {
    map(a, negate_bin_val)
    return(map(a, negate_bin_val))
}

bitwise_and <- function(a, b) {
    bin <- rep(0, times = 16)
    for (pos in 1:16) {
        if (a[[pos]] & b[[pos]]) {
            bin[[pos]] <- 1
        }
    }
    return(bin)
}

bitwise_or <- function(a, b) {
    bin <- rep(0, times = 16)
    for (pos in 1:16) {
        if (a[[pos]] | b[[pos]]) {
            bin[[pos]] <- 1
        }
    }
    return(bin)
}

lshift <- function(a, n) {
    return(c(tail(a, -n), rep(0, times = n)))
}

rshift <- function(a, n) {
    return(c(rep(0, times = n), head(a, -n)))
}

matching_groups <- function(regex, text) {
    matches <- regmatches(text, regexec(regex, text))
    return(tail(matches[[1]], -1))
}

regex_match <- function(regex, text) {
    matches <- regexec(regex, text)
    if (matches[[1]][1] > 0) {
        return(TRUE)
    } else{
        return(FALSE)
    }
}

apply_instruction <- function(instruction, wires, backlog) {

    if (regex_match("(\\w+) RSHIFT (\\d+) -> (\\w+)", instruction)) {
        groups <- matching_groups("(\\w+) RSHIFT (\\d+) -> (\\w+)", instruction)
        groups[[1]] -> source
        groups[[2]] -> shift_amt
        groups[[3]] -> target
        if (exists(source, where = wires)) {
            wires[[target]] <- rshift(wires[[source]], as.integer(shift_amt))
        } else {
            backlog <- append(backlog, instruction)
        }

    } else if (regex_match("(\\w+) LSHIFT (\\d+) -> (\\w+)", instruction)) {
        groups <- matching_groups("(\\w+) LSHIFT (\\d+) -> (\\w+)", instruction)
        groups[[1]] -> source
        groups[[2]] -> shift_amt
        groups[[3]] -> target
        if (exists(source, where = wires)) {
            wires[[target]] <- lshift(wires[[source]], as.integer(shift_amt))
        } else {
            backlog <- append(backlog, instruction)
        }

    } else if (regex_match("(\\w+) AND (\\w+) -> (\\w+)", instruction)) {
        groups <- matching_groups("(\\w+) AND (\\w+) -> (\\w+)", instruction)
        groups[[1]] -> source1
        groups[[2]] -> source2
        groups[[3]] -> target
        if (exists(source1, where = wires) & exists(source2, where = wires)) {
            wires[[target]] <- bitwise_and(wires[[source1]], wires[[source2]])
        } else {
            backlog <- append(backlog, instruction)
        }

    } else if (regex_match("(\\w+) OR (\\w+) -> (\\w+)", instruction)) {
        groups <- matching_groups("(\\w+) OR (\\w+) -> (\\w+)", instruction)
        groups[[1]] -> source1
        groups[[2]] -> source2
        groups[[3]] -> target
        if (exists(source1, where = wires) & exists(source2, where = wires)) {
            wires[[target]] <- bitwise_or(wires[[source1]], wires[[source2]])
        } else {
            backlog <- append(backlog, instruction)
        }

    } else if (regex_match("NOT (\\w+) -> (\\w+)", instruction)) {
        groups <- matching_groups("NOT (\\w+) -> (\\w+)", instruction)
        groups[[1]] -> source
        groups[[2]] -> target
        if (exists(source, where = wires)) {
            wires[[target]] <- bitwise_not(wires[[source]])
        } else {
            backlog <- append(backlog, instruction)
        }

    } else if (regex_match("(\\d+) -> (\\w+)", instruction)) {
        groups <- matching_groups("(\\d+) -> (\\w+)", instruction)
        groups[[1]] -> int_val
        groups[[2]] -> target
        wires[[target]] <- int_to_bin(as.integer(int_val))

    } else if (regex_match("(\\w+) -> (\\w+)", instruction)) {
        groups <- matching_groups("(\\w+) -> (\\w+)", instruction)
        groups[[1]] -> source
        groups[[2]] -> target
        if (exists(source, where = wires)) {
            wires[[target]] <- wires[[source]]
        } else {
            backlog <- append(backlog, instruction)
        }

    } else {
        print("ERROR: instruction could not be parsed.")
        exit()
    }

    return(list(wires, backlog))
}

file <- file("solutions/day7/data.txt", open = "r")
# file <- file("solutions/day7/test.txt", open = "r")
instructions <- readLines(file)
close(file)

for (instruction in instructions) {

    # Apply next instruction
    result <- apply_instruction(instruction, wires, backlog)
    result[[1]] -> wires
    result[[2]] -> backlog

    # Apply any instructions in backlog that can now be applied
    prev_len_backlog <- length(backlog) + 1
    0 -> counter
    while (length(backlog) < prev_len_backlog) {
        counter <- counter + 1
        new_backlog <- list()
        for (instruction in backlog) {
            result <- apply_instruction(instruction, wires, new_backlog)
            result[[1]] -> wires
            result[[2]] -> new_backlog
        }
        prev_len_backlog <- length(backlog)
        backlog <- new_backlog
        if (counter > 0){
            print(counter)
        }
    }

}

for (identifier in names(wires)){
    print(identifier)
    print(bin_to_int(wires[[identifier]]))
}
print(backlog)