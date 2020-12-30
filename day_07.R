data07 <- as.integer(read.table("Input/day07.txt", sep = ",")[1,])
source("run_intcode.R")

#part1-------
in_fun <- function(output, y) {
  envir <- parent.frame()
  if (!exists("ic", envir = envir)) envir$ic <- 1L else envir$ic <- envir$ic + 1L
  if (envir$ic <= length(y)) y[envir$ic] else NULL
}

connect_amp <- function(x) {
  otpt <- 0
  for (i in 1:5) otpt <- run_intcode(data07, in_fun, y = c(x[i], otpt))
  return(otpt)
}

max(unlist(combinat::permn(0:4, connect_amp)))

#part2-----------
connect_amp_loop <- function(x) {
  
  li <- lapply(1:5, function(a) list(intcode = data07, j = 1))
  k <- 1
  k_m1 <- 5L #k minus 1
  s <- 1L
  
  while ("intcode" %in% names(li[[k]])) {
    y <- if (s == 1L) c(x[1], 0L) else if (s < 6) c(x[s], li[[k_m1]]$output) else li[[k_m1]]$output
    
    li[[k]] <- run_intcode(z = li[[k]]$intcode, in_fun, j = li[[k]]$j, y = y,
                           o_fun = function(a) list(output = a))
    k_m1 <- k
    k <- if (k == 5L) 1L else k + 1L 
    s <- s + 1
  }
  return(li[[5]]$output)
}

max(unlist(combinat::permn(5:9, connect_amp_loop)))
