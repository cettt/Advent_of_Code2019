data01 <- read.table("Input/day01.txt")[, 1]

#part1------
sum(floor(data01 / 3) - 2)

#part2----------
det_fuel <- function(x) {
  if (x > 0) max((floor(x / 3) - 2), 0) + det_fuel(floor(x / 3) - 2) else 0
}

sum(sapply(data01, det_fuel))
