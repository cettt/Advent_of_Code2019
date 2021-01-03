library(VeryLargeIntegers)
data22 <- readLines("Input/day22.txt")

a_vec <- as.integer(ifelse(grepl("incr", data22), gsub("\\D", "", data22), 
                           ifelse(grepl("cut", data22), 1, - 1)))
b_vec <- as.integer(ifelse(grepl("cut", data22), gsub("cut ", "", data22),
                           ifelse(grepl("incr", data22), 0, -1)))
b_vec[grepl("cut", data22)] <- -b_vec[grepl("cut", data22)]

compound_ab <- function(ab1, ab2, m) {
  c((ab2[1] * ab1[1]) %% m, (ab2[1] * ab1[2] + ab2[2]) %% m)
}

shuffle_cards <- function(m, n, x, type) {

  ab <- c(a_vec[1] %% m, b_vec[1] %% m)  
  for (j in seq_along(a_vec)[-1]) {
    ab <- compound_ab(ab, c(a_vec[j], b_vec[j]), m)
  }
  
  a <- as.vli(as.character(ab)[1])
  b <- as.vli(as.character(ab)[2])
  m <- as.vli(as.character(m))
  n <- as.vli(as.character(n))
  
  a_n <- powmod(a, n, m) 
  b_n <- mulmod(divmod(submod(a_n, 1, m), submod(a, 1, m), m), b, m)
  
  if (type == "part1") return(summod(mulmod(a_n, x, m), b_n, m))
  return(divmod(submod(x, b_n, m), a_n, m))
}

#part1-----
shuffle_cards(10007, 1, 2019, "part1")

#part2--------
shuffle_cards(m = 119315717514047, n = 101741582076661, 2020, "part2")

#ever shuffle type can be written as a*x + b mod m
# where m is the number of cards, x is the initial (i.e. pre-shuffle) position
# and a and b are constants depending on the shuffle
#   for deal increment d we have that a = d and b = 0
#   for cut c we have that a = 1 and b = -c
#   for deal into new deck we have that a = -1, b = -1
#simple arithmetics show that doing two shuffles back to back can also be written in that form:
# s1(x) = a1*x + b1 mod m, s2(x) = a2*x + b2 mod m
# s(x) = s2(s1(x)) = a1*a2*x + b2 + a2*b1. The function compound ab does exactly this.
# for part2 we need to invert s: s^{-1}(x) = (x - b) / a  mod m
