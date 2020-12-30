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
bfs <- function(.from, .to, maze) { #breath fist search
  queue <- .from
  parent_vec <- NA_complex_
  j <- 1L
  
  while (j <= length(queue)) {
    parent <- queue[j]
    new_edge <- setdiff(maze[abs(parent - maze) == 1], queue)
    if (length(new_edge) > 0) {
      queue <- c(queue, new_edge)
      parent_vec <- c(parent_vec, rep(parent, length(new_edge)))
    }
    if (.to %in%  new_edge) break
    j <- j + 1L
  }
  return(data.frame(z = queue, parent = parent_vec))
}

maze_path <- function(.from_to, maze) {
  queue <- bfs(.from_to[1], .from_to[2], maze)
  path <- .from_to[2]
  while (.from_to[1] != path[1]) {
    parent <- queue[path[1] == queue[, 1], 2]
    path <- c(parent, path)
  }
  
  from_to <- tile2[as.character(.from_to)]
  items <- tile2[names(tile2) %in% as.character(path)]
  
  data.frame(from = from_to[1], to = from_to[2], length = length(path) - 1, 
             keys = paste0("", items[items %in% letters], collapse = ""),
             doors = paste0("", items[!items %in% c(letters, "@")], collapse = ""))
}

find_paths_q <- function(q) {
  q_tiles <- tile2[names(tile2) %in% q$maze]
  q_keys <-  sort(q_tiles[q_tiles %in% c("@", letters)])
  do.call(rbind, combn(as.complex(names(q_keys)), 2, FUN = maze_path, 
                       maze = q$maze, simplify = FALSE))
}

q_paths <- lapply(q_list, find_paths_q)

#part 1---------  
merge_quarters <- function(idx) {
  q1 <- q_paths[[idx[1]]][q_paths[[idx[1]]]$from == "@", ]
  q2 <- q_paths[[idx[2]]][q_paths[[idx[2]]]$from == "@", ]

  add <- if(idx[2] - idx[1] == 2L) 4L else 2L
  f <- function(z) {
    z1 <- q1[q1[, 2] == z[1], ]
    z2 <- q2[q2[, 2] == z[2], ]
    data.frame(from = z[1], to = z[2], length = z1[,3] + z2[,3] + add, 
               keys = paste0(z1[, 4], z2[, 4]), doors = paste0(z1[, 5], z2[, 5]))
  }
  
  do.call(rbind, apply(expand.grid(q1[,2], q2[,2]), 1, f))
}

all_paths1 <- rbind(do.call(rbind, q_paths),
  do.call(rbind, combn(1:4, 2, merge_quarters, simplify = FALSE)))

all_paths1[all_paths1[, 1] == "@", 3] <- all_paths1[all_paths1[, 1] == "@", 3] + 2L

all_paths2 <- all_paths1[, c(2, 1, 3:5)]
colnames(all_paths2)[1:2] <- c("from", "to")
all_paths <- rbind(all_paths1, all_paths2)

hlp_select <- function(.from, rem_k) {
  hlp <- function(z) if (length(z) == 0) TRUE else !any(rem_k %in% tolower(z))
  subset(subset(all_paths, from == .from & to %in% rem_k),
         sapply(strsplit(doors, ""), hlp))
}
  


find_path <- function(.from, rem_k, envir = NULL) {
  if (length(rem_k) == 0) return(0)
  
  if (is.null(envir)) {
    envir <- environment()
    envir$res <- NULL
  }
  
  id <- paste(.from, paste0(sort(rem_k), collapse = ""), sep = "_")
  
  if (id %in% names(envir$res)) return(envir$res[id])
  res <- min(apply(
    hlp_select(.from, rem_k), 1, 
    function(x) {as.numeric(x[3]) + find_path(x[2], setdiff(rem_k, strsplit(x[4], "")[[1]]), envir = envir)}))
  
  envir$res <- c(envir$res, setNames(res, id)) 
  
  return(res) 
}

envir <- environment()
envir$res <- NULL

find_path(.from = "@", rem_k = letters, envir)
