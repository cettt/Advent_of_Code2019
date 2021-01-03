data10 <- sapply(readLines("Input/day10.txt"), function(x) strsplit(x, "")[[1]])

co <- apply(which(data10 == "#", arr.ind = TRUE), 1, function(z) z[1] - 1 + (z[2] - 1)*1i)

#part 1--------
n_aster <- sapply(co, function(z) length(unique(Arg(co[co != z] - z))))
max(n_aster)

#part2-------
z0 <- co[which.max(n_aster)]
arg <-  Arg(z0 - co[co != z0]) + ifelse(Arg(z0 - co[co != z0]) < Arg(1i), 2*base::pi, 0)
r <- ave(abs(z0 - co[co != z0]), arg, FUN = rank) #rank asteroids for fixed arg

(Re(co[co != z0]) * 100 +  Im(co[co != z0]))[order(r, arg)][200]
