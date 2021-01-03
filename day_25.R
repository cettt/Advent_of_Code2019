data25 <- as.numeric(read.table("Input/day25.txt", sep = ",")[1, ])
source("run_intcode.R")

in_fun <- function(output, command) {
  env <- parent.frame()
  if (!exists("ic", env)) env$ic <- 0L 
  env$ic <- env$ic + 1L
  if (env$ic <= length(command)) command[env$ic] else NULL
}

move_robot <- function(command, intcode = data25, j = 1, base = 0) {
  command <- utf8ToInt(paste0(paste0(command, collapse = "\n"), "\n"))
  run_intcode(intcode, in_fun, command = command, j = j, base = base)
}

#part1-------
command <- c(
  "east", "south", "south", "take hologram", "north", "north" ,"west",
  "south", "take mouse", "west", "take whirled peas", "east", "east",
  "take shell", "west", "north", "west", "north", "north", "west\n", "take semiconductor",
  "east", "south", "west", "north", "south", "south", "take hypercube",
  "north", "east", "south", "west", "take antenna", "south", "take spool of cat6\n",
  "north", "west", "south", "south")

res <- move_robot(command)
items <- strsplit(intToUtf8(move_robot("inv", res$intcode, res$j, res$base)$output), "\n")[[1]]
items <- gsub("- ", "", items[grepl("^- ", items)])

for (n in c(4, 3, 5, 6, 2, 1, 7, 0)) {
  comb <- combn(items, n, simplify = FALSE)
  for(it in comb) {
    msg <- move_robot(c(paste("drop", it), "south"), res$intcode, res$j, res$base)
    if (is.numeric(msg)) break;
  }
  if (is.numeric(msg)) break;
}

as.integer(gsub("\\D", "", intToUtf8(msg)))
