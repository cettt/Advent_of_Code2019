data14 <- strsplit(readLines("Input/day14.txt"), " => ")

item_df <- do.call(rbind, lapply(data14, function(z) {
  input <- strsplit(z[1], ", ")[[1]]
  data.frame(input = gsub("\\d+ ", "", input),
             n_in = as.integer(gsub("\\D+", "", input)),
             output = gsub("\\d+ ", "", z[2]),
             n_out = as.integer(gsub("\\D+", "", z[2])))
}))

#part1--------
how_much_need <- function(inp, n_fuel) {
  if (inp == "FUEL") return(n_fuel)
  with(subset(item_df, input == inp),
       sum(n_in * ceiling(mapply(how_much_need, output, n_fuel) / n_out)))
}

how_much_need("ORE", 1L)

#part2--------------
x1 <- 1e5L
x2 <- 1e7L

while (x2 - x1 > 1) { #do a binary search using the function from part1
  x_new <- (x2 + x1) / 2
  if (how_much_need("ORE", x_new) <= 1e12) x1 <- x_new else x2 <- x_new
}
x1
