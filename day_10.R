data10 <- which(t(apply(read.table("Input/day10.txt", comment.char = ""), 1, 
                        function(x) strsplit(x, "")[[1]]) == "#"), arr.ind = TRUE)

co <- apply(data10, 1, function(z) z[2] - 1 + (z[1] - 1)*1i)

#part 1--------
n_aster <- sapply(co, function(z) length(unique(Arg(co[co != z] - z))))
max(n_aster)

#part2-------
z0 <- co[which.max(n_aster)]
arg <-  Arg(z0 - co[co != z0]) + ifelse(Arg(z0 - co[co != z0]) < Arg(1i), 2*pi, 0)
r <- ave(abs(z0 - co[co != z0]), arg, FUN = rank) #rank asteroids for fixed arg

(Re(co[co != z0]) * 100 +  Im(co[co != z0]))[order(r, arg)][200]
