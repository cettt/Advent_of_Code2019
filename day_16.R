data16 <- as.integer(strsplit(readLines("Input/day16.txt"), "")[[1]])

run_fft <- function(x) {
  pat <- c(0L, 1L, 0L, -1L)
  res <- vector("integer", length(x))
  
  for(j in seq_along(x)) {
    f <- rep_len(rep(pat, each = j), length(x) + 1)[-1]
    res[j] <- as.integer(abs(sum(x * f)) %% 10)
  }
  return(res)
}

#part1----
x <- data16
for (j in 1:100) x <- run_fft(x)
sum(x[1:8]*10^(7:0))

#part2-----
run_fft2 <- function(x, times) {
  for (i in seq_len(times)) x <- rev(abs(cumsum(rev(x))) %% 10)
  sum(x[1:8]*10^(7:0))
}

ofs <- sum(data16[1:7]*10^(6:0))
run_fft2(rep(data16, 1e4)[(ofs + 1):(length(data16) * 1e4)], 100L)
