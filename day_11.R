data11 <- as.numeric(read.table("Input/day11.txt", sep = ",")[1, ])
source("run_intcode.R")

#part1--------
in_fun <- function(output, type = "intcode", start_col = 0) {
  
  if (is.null(output)) return(start_col)

  past_dir <- c(1i, -1i)[output[(1:(length(output) / 2)) * 2] + 1]
  past_cols <- output[-(1:(length(output) / 2)) * 2]
  xy <- c(0*1i, cumsum(1i * cumprod(past_dir)))
  cur_xy <- xy[length(xy)]
  past_xy <- xy[-length(xy)]
  
  if (type == "intcode" & ! cur_xy %in% past_xy) return(0) #input while intcode is running
  else if (type == "intcode") return(past_cols[tail(which(past_xy == cur_xy), 1)])
  
  if (type == "result1") return(length(unique(past_xy))) #output for part 1
  if (type == "result2") return(past_xy[past_cols == 1]) #output for part 2
}

in_fun(run_intcode(data11, input_fun = in_fun), type = "result1")

#part2------
res <- in_fun(run_intcode(data11, input_fun = in_fun, start_col = 1), type = "result2")

plot(res, pch = 15, cex = 3, col = "hotpink2", axes = FALSE, ylim = c(-10, 10), 
     xlab = "", ylab = "")
