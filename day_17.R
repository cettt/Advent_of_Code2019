data17 <- as.integer(read.table("Input/day17.txt", sep = ",")[1, ])
source("run_intcode.R")

res <- run_intcode(data17, o_fun = intToUtf8) #cat(res)
map <- do.call(rbind, lapply(strsplit(res, "\n")[[1]], function(z) strsplit(z, split = "")[[1]]))

co <- apply(which(map != ".", arr.ind = TRUE), 1, function(z) z[2]-1 - (z[1]-1)*1i)

#part1------
isct <- co[sapply(co, function(z) sum(abs(z - co) == 1) == 4)]
sum(abs(Re(isct)) * abs(Im(isct)))

#part2------
.start <- co[which(map[which(map != ".")] != "#")] 
path <- .start
pattern <- NULL
dir <- switch(names(sort(table(map)))[1], "^" = 1i, ">" = 1, "v" -1i, "<" = -1)

while (!all(co %in% path)) {
  x0 <- path[length(path)]
  new_dir <- co[abs(co - x0) == 1 & !co %in% path] - x0 
  n <- which(!seq(from = x0, by = new_dir, length.out = max(dim(map))) %in% co)[1] - 2
  path <- c(path, seq(from = x0, by = new_dir, length.out = n + 1))
  pattern <- c(pattern, if (new_dir / dir == 1i) "L" else "R", as.character(n))
  dir <- new_dir
}

compress_pattern <- function(pattern) {
  for (la in 1:10) {
    for (lb in 1:10) {
      .pat <- paste0(pattern, collapse = ",")
      pa <- paste0(pattern[seq_len(2 * la)], collapse = ",")
      pb <- paste0(pattern[2 * la + seq_len(2 * lb)], collapse = ",")
      pc <- strsplit(gsub("^,+", "", gsub(pa, "", gsub(pb, "", .pat))), ",{2, }")[[1]]
      if (length(unique(pc)) == 1 & all(nchar(c(pa, pb, pc)) <= 20)) {
        pc <- pc[1]
        res <- gsub(pa, "A", gsub(pb, "B", gsub(pc, "C", .pat)))
        if (nchar(res) <= 20) return(list(res = res, pa = pa, pb = pb, pc = pc))
      }
    }
  }
}

c_pattern <- compress_pattern(pattern)
y <- utf8ToInt(paste0(Reduce(function(x, y) paste(x, y, sep = "\n"), c_pattern), "\nn\n"))

in_fun <- function(output, y) {
  envir <- parent.frame()
  if (!exists("ic", envir)) envir$ic <- 0L
  envir$ic <- envir$ic + 1L
  y[envir$ic]
}

run_intcode(c(2L, data17[-1]), in_fun, y = y, o_fun = function(x) tail(x, 1))
