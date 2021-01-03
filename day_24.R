data24 <- sapply(readLines("Input/day24.txt"), function(x) strsplit(x, "")[[1]])
#note that the data is transposed which becomes usefull later (see line 14 and 18)

#part1-------------
co <- apply(expand.grid(1:5, 1:5), 1, function(z) z[2] + z[1] * 1i)

update_map_2d <- function(k, themap) {
  z <- themap[k]
  nob <- sum(themap[which(abs(co - co[k]) == 1)] == "#")
  if (z == "#" & nob != 1) "." else if (z == "." & nob >= 1L & nob <= 2L) "#" else z
}

map_2d <- as.character(data24)
map_vec <- sum(2^(0:24) * (map_2d == "#")) #compute biodiversity rating

while (!map_vec[1] %in% map_vec[-1]) {
  map_2d <- sapply(seq_along(co), update_map_2d, themap = map_2d)
  map_vec <- c(sum(2^(0:24) * (map_2d == "#")), map_vec)
}

map_vec[1]
#part2-----------
lookup2d <- lapply(seq_along(co), function(k) which(abs(co - co[k]) == 1))

map_3d <- rep_len(".", 201 * 25)
map_3d[seq_len(25) + 25*100] <- as.character(data24) #starting square
map_3d[13 + (seq_len(201) - 1) * 25] <- "?" #center tiles

make_lookup3d <- function(k) {
  layer <- floor((k - 1) / 25)
  k_2d <- k - layer * 25
  if (k_2d == 13) return(NULL) 
  res <- lookup2d[[k_2d]] + layer * 25 
  
  if (layer < 200 & k_2d %in% c(8, 12, 14, 18)) {
    if      (k_2d == 8)  res <- c(res, (layer + 1L) * 25 + 1:5)
    else if (k_2d == 18) res <- c(res, (layer + 1L) * 25 + 1:5 + 20)
    else if (k_2d == 12) res <- c(res, (layer + 1L) * 25 + c(1, 6, 11, 16, 21))
    else                 res <- c(res, (layer + 1L) * 25 + c(1, 6, 11, 16, 21) + 4)
  }
  if (layer > 0 & !k_2d %in% c(8, 12, 14, 18)) {
    if (k_2d <= 5)  res <- c(res, (layer - 1) * 25 + 8) #left column
    else if (k_2d >= 21) res <- c(res, (layer - 1) * 25 + 18) #right column
    
    if (k_2d %% 5 == 1) res <- c(res, (layer - 1) * 25 + 12) #top row
    else if (k_2d %% 5 == 0) res <- c(res, (layer - 1) * 25 + 14) #bottom row
  }  
  return(res)
}

update_map_3d <- function(k, themap) {
  z <- themap[k]
  nob <- sum(themap[lookup_3d[[k]]] == "#")
  if (z == "#" & nob != 1) "." else if (z == "." & nob >= 1L & nob <= 2L) "#" else z
}

map_3d <- rep_len(".", 201 * 25)
map_3d[seq_len(25) + 25*100] <- as.character(data24) #starting square
map_3d[13 + (seq_len(201) - 1) * 25] <- "?" #center tiles

lookup_3d <- lapply(seq_along(map_3d), make_lookup3d)

for (i in 1:200) {
  idx <- seq.int(25 * (100 - ceiling(i / 2)) + 1, 25 * (100 + ceiling(i / 2)) + 25)
  map_3d[idx] <- sapply(idx, update_map_3d, themap = map_3d)
}
sum(map_3d == "#")
