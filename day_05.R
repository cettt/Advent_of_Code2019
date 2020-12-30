data05 <- as.integer(read.table("Input/day05.txt", sep = ",")[1,])
source("run_intcode.R")

#part1-------
run_intcode(data05, o_fun = function(x) tail(x, 1))

# part2--------
run_intcode(data05, input_fun = function(x) 5)
