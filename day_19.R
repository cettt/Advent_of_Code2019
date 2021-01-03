data19 <- as.numeric(read.table("Input/day19.txt", sep = ",")[1, ])
source("run_intcode.R")

detect_tb <- function(output, x) {
  if (exists("ic", parent.frame())) return(x[2])
  envir <- parent.frame()
  envir$ic <- 1L
  return(x[1])
}

check_tb <- function(x) run_intcode(data19, detect_tb, x = x)

slow_search <- function(x) {# for the first upper grid
  y <- 0L
  while (check_tb(c(x, y)) == 0 & y < 10) y <- y + 1L
  y_min <- if (y < 10) y else 100L #100 is a dummy value, won't be important later on
  while (check_tb(c(x, y + 1)) == 1 & y < 10) y <- y + 1L
  y_max <- if (y < 10) y else 100L
  
  c(x = x, y_min = y_min, y_max = y_max)
}

tb_list <- vector("list", 50L)

tb_list[1:5] <- lapply(0:4, slow_search)

quick_search <- function(li) {
  x <- li[1] + 1L
  y_min <- li[2]
  y_max <- li[3]
  while (! check_tb(c(x, y_min))) y_min <- y_min + 1L
  while (check_tb(c(x, y_max + 1L))) y_max <- y_max + 1L
  
  return(c(x, y_min, y_max))
}

for (k in 6:50) {
  tb_list[[k]] <- quick_search(tb_list[[k - 1]])
}

#part1-------
sum(sapply(tb_list[1:50], function(z) sum(seq.int(z[2], z[3]) <= 49)))

#part2----------
slope1 <- min(sapply(tb_list, function(z) z[2] / z[1]), na.rm = TRUE)

check_ship_bin <- function(x, size = 100L) {
  if (x - size + 1 < 0) return(FALSE)
  y_min <- floor(slope1 * x) - 3L
  while (! check_tb(c(x, y_min))) y_min <- y_min + 1L
  check_tb(c(x - size + 1, y_min + size - 1))
}

x1 <- 101L
x2 <- 1000L

while(x2 - x1 > 1) {
  x_new <- as.integer((x2 + x1) / 2)
  if (check_ship_bin(x_new)) x2 <- x_new else x1 <- x_new
}

y2 <- floor(slope1 * x2) - 3L
while (! check_tb(c(x2, y2))) y2 <- y2 + 1L
(x2 - 99) * 1e4 + y2
