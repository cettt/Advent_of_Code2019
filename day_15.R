data15 <- as.integer(read.table("Input/day15.txt", sep = ",")[1, ])
source("run_intcode.R")

crawl_maze <- function(output) {
  envir <- parent.frame()
  
  if (!exists("xy", envir)) {
    envir$xy <- 0*1i
    envir$cur_dir <- 1 + 0*1i
  } else if (output == 0L) {
      envir$cur_dir <- envir$cur_dir * 1i
  } else {
      envir$xy <- c(envir$xy[1] + envir$cur_dir, envir$xy)
      envir$cur_dir <- envir$cur_dir * (-1i)
      if (output == 2L) envir$oxygen <- envir$xy[1]
  }
  envir$output <- NULL
  
  if (envir$xy[1] == 0*1i & length(envir$xy) > 1) envir$run <- FALSE
  return(which(c(1i, -1i, -1, 1) == envir$cur_dir))

}

o_fun_maze <- function(output) {
  envir <- parent.frame()
  return(list(maze = unique(envir$xy), oxygen = envir$oxygen))
}

bfs <- function(.from, maze = res$maze) {
  queue <- data.frame(z = .from, parent = NA_complex_, step = 0)
  j <- 1L
  
  while (j <= nrow(queue)) {
    parent <- queue[j, 1]
    new_edge <- maze[abs(parent - maze) == 1]
    new_edge <- new_edge[! new_edge %in% queue[, 1]]
    step <- queue[j, 3] + 1L
    if (length(new_edge) > 0) {
      queue <- rbind(queue, data.frame(z = new_edge, parent = parent, step = step))
    }
    j <- j + 1
  }
  return(queue)
}

maze_path <- function(.from, .to, maze = res$maze) {
  queue <- bfs(.from, maze)
  path <- queue[queue[, 1] == .to, ]
  while (! .from %in% path[, 1]) {
    path <- rbind(path, queue[queue[, 1] == path[nrow(path), 2], ])
  }
  return(path[, 1])
}

#part1----------
res <- run_intcode(data15, input_fun = crawl_maze, o_fun = o_fun_maze)
length(maze_path(.from = res$oxygen, .to = 0*1i, maze = res$maze)) - 1

#part2----------
max(bfs(.from = res$oxygen)$step)
