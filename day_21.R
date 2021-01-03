data21 <- as.numeric(read.table("Input/day21.txt", sep = ",")[1, ])
source("run_intcode.R")

in_fun <- function(output, command) {
  env <- parent.frame()
  if (!exists("ic", env)) env$ic <- 0L 
  env$ic <- env$ic + 1L
  return(command[env$ic])
}

move_robot <- function(command) {
  command <- utf8ToInt(paste0(command, collapse = "\n"))
  tail(run_intcode(data21, in_fun, command = command), 1)
}

#part1------
#if there is a hole three spaces away but not four spaces or a hole at the next space jump
move_robot(c("NOT C J", "OR D T", "AND T J", "NOT A T", "OR T J", "WALK\n"))


#part2--------
#if the third tile is empty but the fourth is not jump
#...unless the fifth tilethe eight tile are empty
#if the firsr tile is empty jump
#...or if the second tile is empty but the forth is not, jump
move_robot(c("NOT C J", "OR D T", "AND T J", "AND E T", "OR H T", 
             "AND T J", "NOT A T", "OR T J", "NOT B T", "AND D T", "OR T J", "RUN\n"))
