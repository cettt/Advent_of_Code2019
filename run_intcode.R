#this is the base function for running intcode and can be usesd starting on Day 9
# z is the intcode programm (usually the problem input)
# input_fun is a function which generates an based on previous outputs

run_intcode <- function(z, input_fun = function(output) 1, o_fun = NULL, j = 1L,
                        ...) {
  cls <- function(x) if (!is.na(x)) x else 0L# own coalesce function

  find_para_val <- function(para_id, j) { #para is either 1 or 2.
    .inst <- floor(z[j] / 100) #this is the instruction without the opcode
    mode <- floor((.inst %% 10^(para_id)) / 10^(para_id - 1))
    .base <- if (mode == 2) base else 0

    if (mode == 1) return(cls(z[j + para_id])) #immediate mode
    else return(cls(z[cls(z[j + para_id]) + 1L + .base])) #postion mode resp. relative mode
  }

  output <- NULL
  base <- 0L
  run <- TRUE

  while (run) {
    # print(j)
    opcode <- z[j] %% 100L #first two digits contain the opcode

    if (!opcode %in% c(3L, 99L)) {
      para1 <- find_para_val(1L, j)
      if (!opcode %in% c(4L, 9L)) para2 <- find_para_val(2L, j)
    }

    dummy_base <- if (z[j] >= 2e4) base else 0L #check if output is if in relative mode

    if (opcode == 1L) { #addition, multiplication
      z[z[j + 3L] + 1L + dummy_base] <- para1 + para2
      j <- j + 4L
    }
    else if (opcode == 2L) { #addition, multiplication
      z[z[j + 3L] + 1L + dummy_base] <- para1 * para2
      j <- j + 4L
    }
    else if (opcode == 3L) { #input
      dummy_base <- if (z[j] %% 1e3L >= 200L) base else 0L
      
      inp <- input_fun(output, ...)
      if (!is.null(inp)) {
        z[z[j + 1L] + 1L + dummy_base] <- inp
        j <- j + 2L
      } else {
        return(list(intcode = z, j = j, output = output))
      }
    }
    else if (opcode == 4L) { #output
      output <- c(output, para1)
      j <- j + 2L
    }
    else if (opcode == 5L) { #jump if true/false
      j <- if (para1 != 0L) para2 + 1L else j + 3L
    }
    else if (opcode == 6L) { #jump if true/false
      j <- if (para1 == 0L) para2 + 1L else j + 3L
    }
    else if (opcode == 7L) { #less than
      z[z[j + 3L] + 1L + dummy_base] <- (para1 < para2)
      j <- j + 4L
    }
    else if (opcode == 8L) { #less than
      z[z[j + 3L] + 1L + dummy_base] <- (para1 == para2)
      j <- j + 4L
    }
    else if (opcode == 9L) { #rebase
      base <- base + para1
      j <- j + 2L
    }
    else if (opcode == 99L) break # stop

  }

  if (is.null(o_fun)) output else o_fun(output)
}
