library(tidyverse)

list1 <- list(h1 = c(2,1,3,1), h2 = c(7,1,2), h3 = c(6,22,11,123,4))
list2 <- list(h1 = c(4,6,3,2,1), h2 = c(7,1,2,8,7), h3 = c(16,2,51,23,45))
list3 <- map2(list1, list2, ~ c(.x, .y))
list3 <- map(list3, unique)


foo1 <- tibble(horizon = 1, id = list3[[1]])
foo1

foo2 <- tibble(horizon = 2, id = list3[[2]])
foo2

foo3 <- tibble(horizon = 3, id = list3[[3]])
foo3

foo <- rbind(foo1, foo2, foo3)
foo

moo <- tibble()
for (i in 1:3) {
  this_moo <- tibble(horizon = i, id = list3[[i]])
  moo <- rbind(moo, this_moo)
}

moo
