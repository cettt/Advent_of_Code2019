data12 <- read.table("Input/day12.txt", sep =";")[, 1]

x <- as.integer(gsub(".*x=(-?\\d+).*", "\\1", data12))
y <- as.integer(gsub(".*y=(-?\\d+).*", "\\1", data12))
z <- as.integer(gsub(".*z=(-?\\d+).*", "\\1", data12))

vx <- rep(0L, 4L)
vy <- vx
vz <- vx

#part1-------
simulate_moon <- function(n) {
  
  for (j in 1:n) {
    vx <- vx + colSums(outer(x, x, FUN = function(x, y) sign(x - y)))
    vy <- vy + colSums(outer(y, y, FUN = function(x, y) sign(x - y)))
    vz <- vz + colSums(outer(z, z, FUN = function(x, y) sign(x - y)))
    
    x <- x + vx
    y <- y + vy
    z <- z + vz
  }
  
  sum((abs(x) + abs(y) + abs(z)) * (abs(vx) + abs(vy) + abs(vz)))
}

simulate_moon(1000L)

#part2---------
simulate_moon2 <- function(w0, v0) {
  counter <- 1L
  
  v <- v0 + colSums(outer(w0, w0, FUN = function(x, y) sign(x - y)))
  w <- w0 + v
  while (any(w != w0) | any(v != v0)) {
    v <- v + colSums(outer(w, w, FUN = function(x, y) sign(x - y)))
    w <- v + w
    counter <- counter + 1L
  }
  counter
}

res <- sapply(list(x, y, z), simulate_moon2, v0 = rep(0L, 4))
print(Reduce(pracma::Lcm, res), 16)
