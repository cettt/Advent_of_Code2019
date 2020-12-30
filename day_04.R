data04 <- as.integer(read.table("Input/day04.txt", sep = "-")[1, ])

#part 1-----
check_number1 <- function(x) {
  all(x[1] <= x[2], x[2] <= x[3], x[3] <= x[4], x[4] <= x[5], x[5] <= x[6],
      any(x[1] == x[2], x[2] == x[3], x[3] == x[4], x[4] == x[5], x[5] == x[6]))
}

p1 <- lapply(strsplit(as.character(seq.int(data04[1], data04[2])), ""), as.integer)
p1 <- p1[sapply(p1, check_number1)]
length(p1)

#part2-----
check_number2 <- function(x) {
  any(all(x[1] == x[2], x[2] < x[3]), all(x[1] < x[2], x[2] == x[3], x[3] < x[4]),
      all(x[2] < x[3], x[3] == x[4], x[4] < x[5]), 
      all(x[3] < x[4], x[4] == x[5], x[5] < x[6]), all(x[5] == x[6], x[4] < x[5]))
}
sum(sapply(p1, check_number2))
