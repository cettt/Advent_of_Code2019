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
  queue <- .from
  parent_vec <- NA_complex_
  j <- 1L
  step <- 0L
  
  while (j <= length(queue)) {
    parent <- queue[j]
    new_edge <- setdiff(maze[abs(parent - maze) == 1], queue)
    queue <- c(queue, new_edge)
    parent_vec <- c(parent_vec, rep(parent, length(new_edge)))
    step <- c(step, rep(step[j] + 1L, length(new_edge)))
    j <- j + 1L
  }
  return(list(z = queue, parent = parent_vec, step = step))
}

maze_path <- function(.from, .to, queue) {
  path <- .to
  while (.from != path[1]) path <- c(queue$parent[which(queue$z == path[1])], path)
  return(length(path) - 1L)
}

#part1----------
res <- run_intcode(data15, input_fun = crawl_maze, o_fun = o_fun_maze)
queue <- bfs(res$oxygen)
maze_path(.from = res$oxygen, .to = 0*1i, queue)

#part2----------
max(queue$step)
