data23 <- as.numeric(read.table("Input/day23.txt", sep = ",")[1, ])
source("run_intcode.R")

in_fun <- function(output, y) {
  envir <- parent.frame()
  if (!exists("ic", envir = envir)) envir$ic <- 1L else envir$ic <- envir$ic + 1L
  if (envir$ic <= length(y)) y[envir$ic] else NULL
}

run_nic <- function(type) {
  li <- lapply(0:49, function(a) run_intcode(z = data23, in_fun, y = a))
  k <- 1
  queue <- NULL
  nat <- NULL
  nat_sent <- NULL
  idle_counter <- 0L
  
  while (TRUE) {
    q_addr <- queue[seq_along(queue) %% 3 == 1]
    if ((k - 1) %in% q_addr) {
      y <- queue[(which((k - 1) == q_addr)[1] - 1) * 3 + 2:3]
      queue <- queue[-((which((k - 1) == q_addr)[1] - 1) * 3 + 1:3)]
    } else if (idle_counter == 50) {
      k <- 1L
      y <- nat[2:3]
      if (isTRUE(nat[3] == nat_sent[1])) return(nat[3])
      nat_sent <- c(nat[3], nat_sent)
    } else y <- -1
    
    idle_counter <- if (y[1] == -1) idle_counter + 1L else 0L
    
    li[[k]] <- with(li[[k]], run_intcode(z = intcode, in_fun, j = j, y = y, base = base))
    res <- li[[k]]$output
    if (!is.null(res)) {
      if (res[1] == 255 & type == "part1") return(res[3]) 
      else if (res[1] == 255) nat <- res
      else queue <- c(queue, res)
    }
    k <- if (k < 50) k + 1 else 1
  }
}

#part 1----
run_nic("part1")

#part 2----
run_nic("part2")
