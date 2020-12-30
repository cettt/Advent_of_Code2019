data08 <- matrix(strsplit(read.table(
  "Input/day08.txt", colClasses = "character")[, 1], "")[[1]], nr = 25 * 6)

#part1----------
prod(table(data08[, which.min(apply(data08, 2, function(z) sum(z == "0")))])[-1])

#part2----------
im <- matrix(apply(data08, 1, function(z) z[z != "2"][1]), nc = 25, byrow = TRUE)[6:1, ]

plot(1,1, type = "n", xlim = c(1, 25), ylim = c(-2, 9), axes = F, xlab = "", ylab ="")
points(which(im == "1", arr.ind = TRUE)[, 2:1], pch = 15, cex = 4, col = "hotpink")
