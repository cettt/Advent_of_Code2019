data13 <- as.integer(read.table("Input/day13.txt", sep = ",")[1, ])
source("run_intcode.R")

#part1-------
run_intcode(data13, o_fun = function(x) table(x[seq_along(x) %% 3 == 0])["2"])

# part2--------
in_fun <- function(output) {
  envir <- parent.frame()
  if (!exists("pad_x", envir)) {
    idx <- which(output[seq_len(length(output) / 3L) * 3L] == 3)
    envir$pad_x <- output[seq_len(length(output) / 3L) * 3L - 2L][idx]
  }
  
  tiles <- output[seq_len(length(output) / 3L) * 3L]
  idx <- which(tiles == 4L)[sum(tiles == 4L)]
  res <- sign(output[idx * 3L - 2L] - envir$pad_x)
  envir$pad_x <- envir$pad_x + res
  envir$output <- NULL
  res
}

tail(run_intcode(c(2L, data13[-1]), input_fun = in_fun), 1)
