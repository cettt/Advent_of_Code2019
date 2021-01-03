data02 <- as.integer(read.table("Input/day02.txt", sep = ",")[1, ])
source("run_intcode.R")

o_fun <- function(output) parent.frame()$z[1]

run_1202 <- function(noun, verb) {
  run_intcode(c(data02[1], noun, verb, data02[-(1:3)]), 
              o_fun = function(x) parent.frame()$z[1])
}

#part 1---------
run_1202(12L, 2L)

#part2--------
x <- 0
while (TRUE) {
  if (run_1202(floor(x / 100), x %% 100) == 19690720) break
  x <- x + 1
}
x  
