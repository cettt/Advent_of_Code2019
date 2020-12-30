data03 <- strsplit(read.table("Input/day03.txt", sep = ";")[, 1], ",")

make_path <- function(x) {
  move <- setNames(c(1i, 1, -1i, -1), c("U", "R", "D", "L"))
  y <- 0 + 0*1i
  
  for (k in seq_along(x)) {
    dir <- move[gsub("\\d+", "", x[k])]
    n <- as.integer(gsub("\\D", "", x[k]))
    y <- c(y, seq(from = y[length(y)] + dir, length.out = n, by = dir))
  } 
  return(y[-1])
}

paths <- lapply(data03, make_path)
intsct <- Reduce(intersect, paths)

#part 1--------
min(abs(Re(intsct)) + abs(Im(intsct)))

#part2------
s <- lapply(paths, function(z) sapply(intsct, function(y) which(z == y)[1]))
min(s[[1]] + s[[2]])

