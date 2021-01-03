data18 <- do.call(rbind, strsplit(readLines("Input/day18.txt"), ""))

co <- apply(which(data18 != "#", arr.ind = TRUE), 1, function(z) z[2] + z[1]*1i)
tile <- setNames(data18[data18 != "#"], co)

#decompose maze in four quarters: this is mendatory for part 2 and usefull for part 1
q_list <- list(
  q1 = list(maze = co[Im(co) < 41 & Re(co) < 41], start = 40 + 40*1i),
  q2 = list(maze = co[Im(co) < 41 & Re(co) > 41], start = 42 + 40*1i),
  q3 = list(maze = co[Im(co) > 41 & Re(co) > 41], start = 42 + 42*1i),
  q4 = list(maze = co[Im(co) > 41 & Re(co) < 41], start = 40 + 42*1i)
)

tile2 <- c(tile[tile != "."], setNames(rep("@", 4), sapply(q_list, function(x) x$start)))

# maze functions-------------
bfs <- function(.from, .to, maze) { #breadth fist search
  queue <- .from
  parent_vec <- NA_complex_
  j <- 1L
  
  while (j <= length(queue)) {
    parent <- queue[j]
    new_edge <- setdiff(maze[abs(parent - maze) == 1], queue)
    queue <- c(queue, new_edge)
    parent_vec <- c(parent_vec, rep(parent, length(new_edge)))
    if (all(.to %in% queue)) break
    j <- j + 1L
  }
  return(data.frame(z = queue, parent = parent_vec))
}

maze_path <- function(.from_to, queue) { #finds the shortest path from a to b
  path <- .from_to[2]
  while (.from_to[1] != path[1]) {
    path <- c(queue$parent[which(queue$z == path[1])], path)
  }
  
  from_to <- tile2[as.character(.from_to)]
  items <- tile2[names(tile2) %in% as.character(path)]
  
  data.frame(from = from_to[1], to = from_to[2], length = length(path) - 1, 
             keys = paste0("", items[items %in% letters], collapse = ""),
             doors = paste0("", items[!items %in% c(letters, "@")], collapse = ""))
}

find_paths_q <- function(q) { #find all paths within a single quadrant of the map
  q_tiles <- tile2[names(tile2) %in% q$maze]
  q_keys <-  as.complex(names(sort(q_tiles[q_tiles %in% c("@", letters)])))
  n <- length(q_keys)
  bfs_li <- lapply(1:(n-1), function(k) bfs(q_keys[k], q_keys[-(1:k)], maze = q$maze))
  do.call(rbind, combn(1:n, 2, simplify = FALSE,
                       FUN = function(x) maze_path(q_keys[x], bfs_li[[x[1]]])))
}

q_paths <- lapply(q_list, find_paths_q)

#part 1---------  
merge_quarters <- function(idx) { #function to compute distances between tiles in different quarters
  q1 <- subset(q_paths[[idx[1]]], from == "@")
  q2 <- subset(q_paths[[idx[2]]], from == "@")
  #distance between tile x from q1 and tile y from q2 is equal to
  # dist(x, start_q1) + dist(start_q2, y) + add, 
  #  where is the distance from start_q1 to start_q2
  
  add <- if (idx[2] - idx[1] == 2L) 4L else 2L
  f <- function(z) {
    z1 <- subset(q1, to == z[1])
    z2 <- subset(q2, to == z[2])
    c(from = z[1], to = z[2], length = z1[, 3] + z2[, 3] + add, 
      keys = paste0(z1[, 4], z2[, 4]), doors = paste0(z1[, 5], z2[, 5]))
  }
  
  t(apply(unname(expand.grid(q1[, 2], q2[, 2])), 1, f))
}

mirror_paths <- function(p) {
  as.data.frame(rbind(p, setNames(p[, c(2, 1, 3:5)], c("from", "to", colnames(p)[3:5]))))
}

all_paths <- mirror_paths(rbind(do.call(rbind, q_paths),
                                do.call(rbind, combn(1:4, 2, merge_quarters, simplify = FALSE))))

all_paths[, 3] <- with(all_paths, as.numeric(length) + ifelse(from == "@" | to == "@", 2L, 0L))

hlp_filter <- function(thepaths, .from, rk) { 
  #help function to select right subset given starting point (.from) and a set of remaining keys (rk) 
  subset(
    subset(thepaths, from == .from & to %in% rk),
    sapply(strsplit(doors, ""), function(z) if (length(z)) !any(rk %in% tolower(z)) else TRUE))
}

#help function to update remaining keys (rk) given a set of new keys (nk)
upd_keys <- function(rk, nk) setdiff(rk, strsplit(nk, "")[[1]])

find_path <- function(.from, rk, envir = NULL, thepaths) {
  if (length(rk) == 0) return(0)
  id <- paste(.from, paste0(sort(rk), collapse = ""), sep = "_")
  
  if (id %in% names(envir$res)) return(envir$res[id])
  res <- with(hlp_filter(thepaths, .from, rk), min(mapply(
    function(.t, .k, .l) .l + find_path(.t, upd_keys(rk, .k), envir, thepaths),
    to, keys, length)))
  
  envir$res <- c(envir$res, setNames(res, id))
  return(res) 
} 

envir <- environment()
envir$res <- NULL

find_path(.from = "@", rk = letters, envir, all_paths)

#part2------
sum(sapply(q_paths, function(q) find_path("@", unique(q[, 2]), envir, mirror_paths(q))))
