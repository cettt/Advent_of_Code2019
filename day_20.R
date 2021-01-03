data20 <- do.call(rbind, strsplit(readLines("Input/day20.txt"), ""))

co_all <- apply(which(data20 != " ", arr.ind = TRUE), 1, function(z) z[2] + z[1]*1i) 
names(co_all) <- as.character(data20)[as.character(data20) != " "]

#parsing fun :/---------
name_portal <- function(z) {
  
  nei <- co_all[abs(co_all - z) <= 1]
  if (! "." %in% names(nei)) return(NULL)
  
  .p <- nei[names(nei) == "."]  
  nei <- nei[names(nei) != "."]
  f <- if (Im(.p) == Im(z)) Re else Im 
  setNames(.p, paste0(names(nei)[order(f(nei))], collapse = ""))
}

port <- unlist(sapply(unname(co_all[names(co_all) %in% LETTERS]), name_portal))
co <- apply(which(data20 == ".", arr.ind = TRUE), 1, function(z) z[2] + z[1]*1i) 

#part 1--------
link_portal_2d <- function(z) {
  if (names(port[port == z]) %in% c("AA", "ZZ")) return(z)
  port[names(port) == names(port[port == z]) & port != z]
}

port2d_linked <- setNames(port, sapply(port, link_portal_2d))

bfs <- function(.from, maze = co) {#bfs without portals
  queue <- .from
  parent_vec <- NA_complex_
  j <- 1L
  
  while (j <= length(queue)) {
    parent <- queue[j]
    new_edge <- setdiff(maze[abs(parent - maze) == 1], queue)
    queue <- c(queue, new_edge)
    parent_vec <- c(parent_vec, rep(parent, length(new_edge)))
    j <- j + 1L
  }
  return(data.frame(z = queue, parent = parent_vec))
}

maze_path <- function(.from, .to, queue) {#find shortest paths without portals
  path <- .to
  while (.from != path[1]) {
    path <- c(queue$parent[which(queue$z == path[1])], path)
  }
  data.frame(from = .from, to = .to, d = length(path) - 1)
}

bfs_list <- lapply(port, bfs, maze = co)
walk_dest <- lapply(bfs_list, function(z) port[port %in% z[-1, 1]]) #tiles that can be reached without portals

maze_2d <- do.call(rbind, lapply(
  seq_along(port), function(i) 
    do.call(rbind, lapply(walk_dest[[i]], function(x) maze_path(port[i], x, bfs_list[[i]])))
))

find_path <- function(.from, .to, prt, mz) {
  if (.from == .to) return(-1)
  envir$visited <- c(envir$visited, .from, prt[as.character(.from)])
  if (nrow(subset(mz, from == .from & !to %in% envir$visited)) == 0) return(Inf)
  with(subset(mz, from == .from & !to %in% envir$visited), min(mapply(
    function(.t, .d) .d + 1 + find_path(prt[as.character(.t)], .to, prt, mz),
    to, d)))
}

envir <- environment()
envir$visited <- NULL
find_path(port["AA"], port["ZZ"], port2d_linked, maze_2d)

#part2------
n_level <- length(unique(names(port))) - 2 #the deepest level is the number of teleports

o_port <- port[ Re(port) %in% range(Re(port)) |  Im(port) %in% range(Im(port))]
i_port <- port[!Re(port) %in% range(Re(port)) & !Im(port) %in% range(Im(port))]

port_3d <- unlist(lapply(seq_len(n_level) - 1L, function(n) unname(port + n * 1e3 * (1 + 1i))))

link_portal_3d <- function(z) {
  lvl <- ceiling(Re(z) / 1000)
  z0 <- z - (lvl - 1) * 1e3* (1 + 1i)
  new_lvl <- lvl + if (z0 %in% o_port) - 1 else + 1
  if (new_lvl < 1 | new_lvl > n_level) return(z)
  if (names(port[port == z0]) %in% c("AA", "ZZ")) return(z)
  port[names(port) == names(port[port == z0]) & port != z0] + (new_lvl - 1) * 1e3 * (1 + 1i)
}

port3d_linked <- setNames(port_3d, sapply(port_3d, link_portal_3d))

maze_3d <- do.call(rbind, lapply(seq_len(n_level) - 1,function(lvl) {
  cbind(maze_2d[, 1:2] + lvl*1000*(1+1i), d = maze_2d[,3]) 
}))

envir <- environment()
envir$visited <- NULL
find_path(port["AA"], port["ZZ"], port3d_linked, maze_3d)
