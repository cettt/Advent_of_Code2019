data06 <- read.table("Input/day06.txt", sep = ")")

#part1-----
nor <- vector("integer", nrow(data06))
x0 <- "COM"
k <- 1L

while (any(nor == 0)) {
  idx <- which(data06[, 1] %in% x0)
  nor[idx] <- k
  x0 <- data06[idx, 2]
  k <- k + 1L
}
sum(nor)

#part2-------
map_from <- function(x) {
  if (x != "COM") c(x, map_from(data06[which(data06[, 2] == x), 1])) else x
}

table(table(c(map_from("YOU"), map_from("SAN"))))[1] - 2L
