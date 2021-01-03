data09 <- as.numeric(read.table("Input/day09.txt", sep = ",")[1, ])
source("run_intcode.R")

#part1-------
run_intcode(data09, input_fun = function(x) 1)

# part2--------
run_intcode(data09, input_fun = function(x) 2)
